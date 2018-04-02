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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.LibLTDL (
    Advice(..),
    DLException(..),
    DLHandle(..),
    SearchPath,
    dlInit,
    dlExit,
    dlOpen,
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
import Control.Monad (liftM)
import Data.List (foldl')
import Data.Typeable
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.LibLTDL.Internal

type SearchPath = [FilePath]

foreign import ccall "wrapper"
    mkCallback :: (CString -> Ptr a -> IO CInt) -> IO (FunPtr (CString -> Ptr a -> IO CInt))

-- |LibLTDL extensible exception.
newtype DLException = DLException String
  deriving (Show, Typeable)

instance Exception DLException

withNULLCString :: Maybe String -> (CString -> IO a) -> IO a
withNULLCString Nothing f  = f nullPtr
withNULLCString (Just s) f = withCString s f

withCSearchPath :: SearchPath -> (CString -> IO a) -> IO a
withCSearchPath path f =
    withCString (foldl' (\x y -> x ++ lT_PATHSEP_CHAR : y) [] path) f

withNULLCSearchPath :: Maybe SearchPath -> (CString -> IO a) -> IO a
withNULLCSearchPath Nothing f =
    f nullPtr

withNULLCSearchPath (Just path) f =
    withCString (foldl' (\x y -> x ++ lT_PATHSEP_CHAR : y) [] path) f

checkDLResult :: CInt -> IO ()
checkDLResult i
    | i < 0     = dlError
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
-- may be called several times.
dlInit :: IO ()
dlInit = c_lt_dlinit >>= checkDLResult

-- |Shut down libltdl and close all modules. This function will only then shut
-- down libltdl when it was called as many times as @dlInit@ has been
-- successfully called.
dlExit :: IO ()
dlExit = c_lt_dlinit >>= checkDLResult

-- |Open the module with the specified file name and return a handle for it.
dlOpen :: Maybe String -> IO DLHandle
dlOpen = checkDLOpenResult c_lt_dlopen

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
-- value returned by the last call to @f@ is returned.
dlForEachFile :: Maybe SearchPath -> (String -> IO Int) -> IO Int
dlForEachFile path f =
    withNULLCSearchPath path $ \cpath -> do
    callback' <- mkCallback callback
    result <- c_lt_dlforeachfile cpath callback' nullPtr
    freeHaskellFunPtr callback'
    return (fromIntegral result)
  where
    callback :: CString -> Ptr a -> IO CInt
    callback cfilename _ =
        peekCString cfilename >>= f >>= return . fromIntegral

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
