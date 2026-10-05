{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import           Control.Concurrent       (forkIO, killThread, myThreadId,
                                           newEmptyMVar, putMVar, takeMVar,
                                           tryPutMVar, yield)
import           Control.Exception        (AsyncException (ThreadKilled),
                                           IOException, MaskingState (..),
                                           SomeException, bracket, bracket_,
                                           fromException, getMaskingState,
                                           mask_, throwIO, try,
                                           uninterruptibleMask_)
import           Control.Monad            (when)
import           Foreign.C.String         (CString, withCString)
import           Foreign.C.Types          (CInt (..))
import           Foreign.LibLTDL
import           Foreign.LibLTDL.Internal (c_lt_dlexit)
import           Foreign.Ptr              (FunPtr, Ptr, nullFunPtr, nullPtr)
import           GHC.Conc                 (ThreadStatus (..), threadStatus)
import           System.Directory        (createDirectory, getTemporaryDirectory,
                                           removeDirectoryRecursive, removeFile)
import           System.IO               (hClose, openTempFile)
import           System.Info              (os)
import           System.IO.Error          (ioeGetErrorString, isUserError)
import           Test.Tasty               (TestTree, defaultMain, localOption,
                                           testGroup)
import           Test.Tasty.HUnit         (assertBool, assertFailure, testCase,
                                           (@?=))
import           Test.Tasty.Runners       (NumThreads (..))

main :: IO ()
main = defaultMain $ localOption (NumThreads 1) $ testGroup "libltdl"
    [ testCase "shutdown balances initialization" $ freshLibrary $ do
        dlInit
        dlExit
        status <- c_lt_dlexit
        assertBool "libltdl is still initialized after dlExit" (status /= 0)
    , testCase "shutdown without initialization raises DLException" $ freshLibrary $ do
        result <- try dlExit :: IO (Either DLException ())
        case result of
            Left _ -> return ()
            Right () -> assertFailure "Shutdown succeeded without initialization"
    , testCase "search path round trip" $ withLibrary $ do
        let paths = ["/libltdl-test/one", "/libltdl-test/two"]
        dlSetSearchPath paths
        dlGetSearchPath >>= (@?= paths)
    , testCase "empty search path" $ withLibrary $ do
        dlSetSearchPath []
        dlGetSearchPath >>= (@?= [])
    , testCase "main program is resident and cannot be closed" $ withLibrary $ do
        handle <- dlOpen Nothing
        dlIsResident handle >>= (@?= True)
        result <- try (dlClose handle) :: IO (Either DLException ())
        case result of
            Left _   -> return ()
            Right () -> assertFailure "Closed a resident module"
    , testCase "missing module raises DLException" $ withLibrary $ do
        result <- try (dlOpen (Just "/libltdl-test/nonexistent-module"))
            :: IO (Either DLException DLHandle)
        case result of
            Left _ -> return ()
            Right handle -> do
                dlClose handle
                assertFailure "Opened a nonexistent module"
    , callbackTests
    , testCase "conflicting advice flags raise DLException" $ withLibrary $ do
        result <- try (dlOpenAdvise [Global, Local] Nothing)
            :: IO (Either DLException DLHandle)
        case result of
            Left _  -> return ()
            Right _ -> assertFailure "Accepted conflicting visibility advice"
    , testGroup "withDL" $
        [ testCase "resident module scope returns its result and shuts down" $ freshLibrary $ do
            resident <- withDL Nothing dlIsResident
            resident @?= True
            assertStopped
        , testCase "failed opening still shuts down" $ freshLibrary $ do
            result <- try (withDL (Just "/libltdl-test/nonexistent-module") (\_ -> return ()))
                :: IO (Either DLException ())
            case result of
                Left _   -> return ()
                Right () -> assertFailure "Opened a nonexistent module"
            assertStopped
        , testCase "callback exception still shuts down" $ freshLibrary $ do
            result <- try (withDL Nothing (\_ -> throwIO (userError "callback failed")))
                :: IO (Either IOException ())
            assertCallbackException result
            assertStopped
        ] ++ ordinaryModuleTests
    ]

-- The Linux tests use the libltdl module whose development files are required
-- to build this package. Keep another initialization active so shutdown cannot
-- hide a missing dlClose call.
ordinaryModuleTests :: [TestTree]
ordinaryModuleTests
    | os /= "linux" = []
    | otherwise     =
        [ testCase "advice extension search is opt-in" $ withLibrary $ do
            result <- try (dlOpenAdvise [] (Just "libltdl"))
                :: IO (Either DLException DLHandle)
            case result of
                Left _ -> return ()
                Right handle -> do
                    dlClose handle
                    assertFailure "Appended an extension without Ext advice"
            bracket (dlOpenAdvise [Ext] (Just "libltdl")) dlClose $ \handle ->
                dlIsResident handle >>= (@?= False)
        , testCase "ordinary module is closed after returning" $ withLibrary $ do
            before <- loadedModules
            result <- withDL (Just "libltdl.so") $ \handle -> do
                dlIsResident handle >>= (@?= False)
                loadedModules >>= (@?= before + 1)
                return (42 :: Int)
            result @?= 42
            loadedModules >>= (@?= before)
        , testCase "ordinary module is closed after a callback exception" $ withLibrary $ do
            before <- loadedModules
            result <- try (withDL (Just "libltdl.so") $ \_ -> do
                loadedModules >>= (@?= before + 1)
                throwIO (userError "callback failed"))
                :: IO (Either IOException ())
            assertCallbackException result
            loadedModules >>= (@?= before)
        , testCase "ordinary module is closed after thread cancellation" $ withLibrary $ do
            before <- loadedModules
            started <- newEmptyMVar
            blocked <- newEmptyMVar
            finished <- newEmptyMVar
            thread <- forkIO $ do
                result <- try (withDL (Just "libltdl.so") $ \_ -> do
                    loadedModules >>= (@?= before + 1)
                    putMVar started ()
                    takeMVar blocked) :: IO (Either SomeException ())
                putMVar finished result
                -- Let the test finish if opening or a callback assertion failed
                -- before it could signal that the module was loaded.
                _ <- tryPutMVar started ()
                return ()
            takeMVar started
            killThread thread
            result <- takeMVar finished
            case result of
                Left exception -> case fromException exception of
                    Just ThreadKilled -> return ()
                    _ -> assertFailure ("Unexpected exception: " ++ show exception)
                Right () -> assertFailure "Callback returned before cancellation"
            loadedModules >>= (@?= before)
        ]

assertCallbackException :: Either IOException () -> IO ()
assertCallbackException result =
    case result of
        Left exception -> do
            assertBool "Callback raised a different IO error" (isUserError exception)
            ioeGetErrorString exception @?= "callback failed"
        Right ()       -> assertFailure "Callback exception was swallowed"

-- Observe the native module list rather than trying to use a stale handle.
foreign import ccall "ltdl.h lt_dlinterface_register"
    registerInterface :: CString -> FunPtr (DLHandle -> CString -> IO CInt) -> IO (Ptr ())

foreign import ccall "ltdl.h lt_dlinterface_free"
    freeInterface :: Ptr () -> IO ()

foreign import ccall "ltdl.h lt_dlhandle_iterate"
    nextHandle :: Ptr () -> DLHandle -> IO DLHandle

loadedModules :: IO Int
loadedModules =
    withCString "libltdl-tests" $ \name ->
      bracket (registerInterface name nullFunPtr) freeInterface $ \interface -> do
        assertBool "Failed to register a module interface" (interface /= nullPtr)
        count interface (DLHandle nullPtr)
  where
    count interface previous = do
        handle <- nextHandle interface previous
        if unDLHandle handle == nullPtr
          then return 0
          else fmap (+ 1) (count interface handle)

withLibrary :: IO a -> IO a
withLibrary action = freshLibrary $ do
    result <- bracket_ dlInit dlExit action
    assertStopped
    return result

assertStopped :: IO ()
assertStopped = do
    status <- c_lt_dlexit
    assertBool "libltdl is still initialized" (status /= 0)

freshLibrary :: IO a -> IO a
freshLibrary = bracket_ drainLibrary drainLibrary

-- Use the C shutdown function to isolate tests even when the Haskell wrapper
-- fails to balance initialization.
drainLibrary :: IO ()
drainLibrary = do
    status <- c_lt_dlexit
    when (status == 0) drainLibrary

callbackTests :: TestTree
callbackTests = testGroup "dlForEachFile"
    [ testCase "returns the callback result and stops iteration" $
        withLibrary $ withModules $ \path -> do
            result <- dlForEachFile (Just [path]) (\_ -> return 7)
            result @?= 7
    , testCase "rethrows callback exceptions" $
        withLibrary $ withModules $ \path -> do
            result <- try (dlForEachFile (Just [path]) (\_ -> throwIO (userError "callback failed")))
            assertCallbackException (fmap (const ()) result)
    , testCase "catches exceptions in a lazy callback result" $
        withLibrary $ withModules $ \path -> do
            result <- try (dlForEachFile (Just [path]) (\_ -> return (error "lazy callback result")))
                :: IO (Either SomeException Int)
            case result of
                Left _  -> return ()
                Right _ -> assertFailure "Callback result was not evaluated"
    , testCase "runs the callback in the caller's masking state" $
        withLibrary $ withModules $ \path -> do
            let observe = do
                    state <- newEmptyMVar
                    _ <- dlForEachFile (Just [path]) $ \_ -> do
                        getMaskingState >>= putMVar state
                        return 1
                    takeMVar state
            observe >>= (@?= Unmasked)
            mask_ observe >>= (@?= MaskedInterruptible)
            uninterruptibleMask_ observe >>= (@?= MaskedUninterruptible)
    , testCase "rethrows an exception that the callback leaves pending" $
        withLibrary $ withModules $ \path -> do
            -- Under mask_ the callback function runs masked, so the exception
            -- stays pending until the function has returned.
            result <- try $ mask_ $ dlForEachFile (Just [path]) $ \_ -> do
                callbackThread <- myThreadId
                thrower <- forkIO (killThread callbackThread)
                let wait = do
                        status <- threadStatus thrower
                        when (status == ThreadRunning) (yield >> wait)
                wait
                return 0
            case result of
                Left e | Just ThreadKilled <- fromException e -> return ()
                Left e -> assertFailure ("Unexpected exception: " ++ show e)
                Right n -> assertFailure ("Returned " ++ show (n :: Int))
    , testCase "rethrows thread cancellation outside the native callback" $
        withLibrary $ withModules $ \path -> do
            started <- newEmptyMVar
            blocked <- newEmptyMVar
            finished <- newEmptyMVar
            _ <- forkIO $ do
                result <- try $ dlForEachFile (Just [path]) $ \_ -> do
                    myThreadId >>= putMVar started
                    takeMVar blocked
                putMVar finished (result :: Either SomeException Int)
                -- A safe foreign call may run its callback on a new thread.
                -- Signal the caller if enumeration fails before the callback.
                caller <- myThreadId
                _ <- tryPutMVar started caller
                return ()
            callbackThread <- takeMVar started
            killThread callbackThread
            result <- takeMVar finished
            case result of
                Left exception -> case fromException exception of
                    Just ThreadKilled -> return ()
                    _ -> assertFailure ("Unexpected exception: " ++ show exception)
                Right _ -> assertFailure "Callback returned before cancellation"
            -- The iterator remains usable after cancellation.
            dlForEachFile (Just [path]) (\_ -> return 0) >>= (@?= 0)
    ]

-- Enumeration needs module names, not loadable shared objects.
withModules :: (FilePath -> IO a) -> IO a
withModules = bracket create removeDirectoryRecursive
  where
    create = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "libltdl-callback"
        hClose handle
        removeFile path
        createDirectory path
        writeFile (path ++ "/sample.so") ""
        return path
