-- Copyright (c) 2011
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.LibLTDL (
    Advice(..),
    DLException(..),
    DLHandle(..),
    SearchPath,
    dlInit,
    dlExit,
    dlOpen,
    withDL,
    dlOpenExt,
    dlOpenAdvise,
    dlClose,
    dlSym,
    dlSetSearchPath,
    dlGetSearchPath,
    dlForEachFile,
    dlMakeResident,
    dlIsResident
  ) where

import Control.Exception
import Control.Monad (unless)
import Data.IORef
import qualified Data.List as List (foldl')
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.LibLTDL.Internal

type SearchPath = [FilePath]

foreign import ccall "wrapper"
    mkCallback :: (CString -> Ptr a -> IO CInt) -> IO (FunPtr (CString -> Ptr a -> IO CInt))

-- |LibLTDL extensible exception.
newtype DLException = DLException String
  deriving (Show)

instance Exception DLException

withNULLCString :: Maybe String -> (CString -> IO a) -> IO a
withNULLCString Nothing f  = f nullPtr
withNULLCString (Just s) f = withCString s f

withCSearchPath :: SearchPath -> (CString -> IO a) -> IO a
withCSearchPath path f =
    withCString (List.foldl' (\x y -> x ++ lT_PATHSEP_CHAR : y) [] path) f

withNULLCSearchPath :: Maybe SearchPath -> (CString -> IO a) -> IO a
withNULLCSearchPath Nothing f =
    f nullPtr

withNULLCSearchPath (Just path) f =
    withCString (List.foldl' (\x y -> x ++ lT_PATHSEP_CHAR : y) [] path) f

checkDLResult :: CInt -> IO ()
checkDLResult i
    | i /= 0    = dlError
    | otherwise = return ()

checkDLOpenResult :: (CString -> IO DLHandle) -> Maybe String -> IO DLHandle
checkDLOpenResult dlopen maybe_filename =
    withNULLCString maybe_filename $ \cfilename -> do
    h <- dlopen cfilename
    if unDLHandle h == nullPtr
      then dlError
      else return h

dlError :: IO a
dlError = c_lt_dlerror >>= peekCString >>= throwIO . DLException

-- |Initialize libltdl. This function must be called before using libltdl and
-- may be called several times. Balance each successful call with 'dlExit'.
dlInit :: IO ()
dlInit = c_lt_dlinit >>= checkDLResult

-- |Shut down libltdl. The 'dlExit' call that balances the last successful
-- 'dlInit' call unloads every module that is not resident. Resident modules,
-- including the main program, stay loaded for the life of the process. Do not
-- use handles or symbol pointers after that final call, even those for
-- resident modules. Calling 'dlExit' more times than 'dlInit' raises
-- t'DLException'.
dlExit :: IO ()
dlExit = c_lt_dlexit >>= checkDLResult

-- |Open the module with the specified file name and return a handle for it.
-- 'Nothing' opens the main program, whose resident handle cannot be closed
-- with 'dlClose'.
dlOpen :: Maybe String -> IO DLHandle
dlOpen = checkDLOpenResult c_lt_dlopen

-- |Initialize libltdl, open a module with 'dlOpen', and run an action with its
-- handle. Close the handle and balance initialization with 'dlExit' when the
-- action returns or raises an exception. If opening fails, initialization is
-- still balanced.
--
-- Resident handles, including the main program opened by 'Nothing', cannot be
-- closed with 'dlClose', so they are left open. libltdl never unloads resident
-- modules, even at the final 'dlExit' call.
--
-- The action must not close the handle. Use the handle and any pointers
-- obtained with 'dlSym' only within the action.
--
-- @since 0.2.0
withDL :: Maybe String -> (DLHandle -> IO a) -> IO a
withDL name action =
    bracket_ dlInit dlExit $
      bracket (dlOpen name) close action
  where
    close h = do
        resident <- dlIsResident h
        unless resident (dlClose h)

-- |Open the module with the specified file name and return a handle to it. This
-- variant tries appending various extensions to the file name in an effort to
-- find the module.
dlOpenExt :: Maybe String -> IO DLHandle
dlOpenExt = checkDLOpenResult c_lt_dlopenext

data Advice = Ext
            | Global
            | Local
            | Resident
            | Preload
  deriving (Eq, Ord, Enum, Show)

withAdvice :: [Advice] -> (Ptr DLAdvise -> IO a) -> IO a
withAdvice flags f =
    alloca $ \advPtr -> do
    c_lt_dladvise_init advPtr >>= checkDLResult
    c_lt_dladvise_ext advPtr >>= checkDLResult
    (mapM_ (setFlag advPtr) flags >> f advPtr)
      `finally` (c_lt_dladvise_destroy advPtr >>= checkDLResult)
  where
    setFlag :: Ptr DLAdvise -> Advice -> IO ()
    setFlag advPtr Ext      = c_lt_dladvise_ext advPtr >>= checkDLResult
    setFlag advPtr Global   = c_lt_dladvise_global advPtr >>= checkDLResult
    setFlag advPtr Local    = c_lt_dladvise_local advPtr >>= checkDLResult
    setFlag advPtr Resident = c_lt_dladvise_resident advPtr >>= checkDLResult
    setFlag advPtr Preload  = c_lt_dladvise_preload advPtr >>= checkDLResult

dlOpenAdvise :: [Advice] -> Maybe String -> IO DLHandle
dlOpenAdvise flags name =
    withAdvice flags $ \advPtr -> do
    adv <- peek advPtr
    checkDLOpenResult (\cname -> c_lt_dlopenadvise cname adv) name

-- |Decrement the reference count on the module @h@. If it drops to zero and no
-- other module depends on this module, then the module is unloaded.
-- Closing a resident module raises t'DLException'.
dlClose :: DLHandle -> IO ()
dlClose h =
    c_lt_dlclose h >>= checkDLResult

-- |Return the address of the symbol @name@ in the module @h@.
dlSym :: DLHandle -> String -> IO (FunPtr a)
dlSym h name =
    withCString name $ \cname -> do
        fptr <- c_lt_dlsym h cname
        if fptr == nullFunPtr
          then dlError
          else return fptr

-- |Replace the current user-defined library search path with @path@, which
-- must be a list of absolute directories separated by @lT_PATHSEP_CHAR@.
dlSetSearchPath :: SearchPath -> IO ()
dlSetSearchPath path =
    withCSearchPath path $ \cpath ->
        c_lt_dlsetsearchpath cpath >>= checkDLResult

-- |Return the current user-defined library search path.
dlGetSearchPath :: IO SearchPath
dlGetSearchPath = do
    cpath <- c_lt_dlgetsearchpath
    if cpath == nullPtr
      then return []
      else peekCString cpath >>= return . paths
  where
    paths :: String -> SearchPath
    paths s =
        let (p, s') = break (== lT_PATHSEP_CHAR) s
        in  p : case s' of
                  []      -> []
                  (_:s'') -> paths s''

-- |Iterate over the directory list in @path@, calling @f@ for each module,
-- until @f@ returns a non-zero result, or until there are no more modules. The
-- value returned by the last call to @f@ is returned. Exceptions that the
-- callback raises, and asynchronous exceptions that reach its thread while it
-- runs, stop iteration and are rethrown after the native iterator returns.
-- They never unwind through the C call. The callback runs in the masking state
-- of the thread that called 'dlForEachFile'. An exception thrown to that
-- thread, such as the one that @System.Timeout.timeout@ sends, does not
-- interrupt iteration. It is raised only after the native iterator returns.
dlForEachFile :: Maybe SearchPath -> (String -> IO Int) -> IO Int
dlForEachFile path f =
    withNULLCSearchPath path $ \cpath -> mask $ \restore -> do
        failure <- newIORef (Nothing :: Maybe SomeException)
        let -- GHC runs each callback on a new Haskell thread, which starts
            -- unmasked whatever the masking state of this thread. Mask the
            -- whole callback and run only the user function in this thread's
            -- masking state. The handler also catches an exception that was
            -- pending while the callback was masked, which is delivered when
            -- the mask ends. No handler can catch one that arrives after that,
            -- as the callback returns to C.
            callback cfilename _ = handle record $ mask_ $ do
                previous <- readIORef failure
                case previous of
                    Just _  -> return 1
                    Nothing -> restore $ do
                        filename <- peekCString cfilename
                        f filename >>= evaluate . fromIntegral
            -- Keep the first exception and stop the native iteration.
            record exception = do
                modifyIORef' failure (maybe (Just exception) Just)
                return 1
        bracket (mkCallback callback) freeHaskellFunPtr $ \callback' -> do
            result <- c_lt_dlforeachfile cpath callback' nullPtr
            exception <- readIORef failure
            case exception of
                Just e  -> throwIO e
                Nothing -> return (fromIntegral result)

-- |Mark a module so that it cannot be closed.
dlMakeResident :: DLHandle -> IO ()
dlMakeResident h =
    c_lt_dlmakeresident h >>= checkDLResult

-- |Check whether a particular module has been marked as resident, return @True@
-- if it has or @False@ otherwise.
dlIsResident :: DLHandle -> IO Bool
dlIsResident h = do
    result <- c_lt_dlisresident h
    case result of
      1 -> return True
      _ -> return False
