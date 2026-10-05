module Main (main) where

import           Control.Exception        (bracket_, try)
import           Control.Monad            (when)
import           Foreign.LibLTDL
import           Foreign.LibLTDL.Internal (c_lt_dlexit)
import           Test.Tasty               (defaultMain, localOption, testGroup)
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
    ]

withLibrary :: IO a -> IO a
withLibrary action = freshLibrary $ bracket_ dlInit dlExit action

freshLibrary :: IO a -> IO a
freshLibrary = bracket_ drainLibrary drainLibrary

-- Use the C shutdown function to isolate tests even when the Haskell wrapper
-- fails to balance initialization.
drainLibrary :: IO ()
drainLibrary = do
    status <- c_lt_dlexit
    when (status == 0) drainLibrary
