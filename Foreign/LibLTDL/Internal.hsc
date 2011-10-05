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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Foreign.LibLTDL.Internal where

import Foreign
import Foreign.C.String
import Foreign.C.Types

#let const_char c = "'%c'", c

#include <ltdl.h>

-- |System-dependent path separator
lT_PATHSEP_CHAR :: Char
lT_PATHSEP_CHAR = #{const_char LT_PATHSEP_CHAR}

-- |Module handle
data OpaqueDLHandle = OpaqueDLHandle

newtype DLHandle = DLHandle { unDLHandle :: Ptr OpaqueDLHandle }
  deriving (Eq, Ord, Show)

-- |Optional module loading modes
data OpaqueDLAdvise = OpaqueDLAdvise

newtype DLAdvise = DLAdvise { unDLAdvise :: Ptr OpaqueDLAdvise }
  deriving (Eq, Ord, Show, Storable)

-- Initialization
foreign import ccall "libltdl.h lt_dlinit"
    c_lt_dlinit :: IO CInt

foreign import ccall "libltdl.h lt_dlexit"
    c_lt_dlexit :: IO CInt

foreign import ccall "libltdl.h lt_dlopen"
    c_lt_dlopen :: CString -> IO DLHandle

foreign import ccall "libltdl.h lt_dlopenext"
    c_lt_dlopenext :: CString -> IO DLHandle

foreign import ccall "libltdl.h lt_dlopenadvise"
    c_lt_dlopenadvise :: CString -> DLAdvise -> IO DLHandle

foreign import ccall "libltdl.h lt_dladvise_init"
    c_lt_dladvise_init :: Ptr DLAdvise -> IO CInt

foreign import ccall "libltdl.h lt_dladvise_destroy"
    c_lt_dladvise_destroy :: Ptr DLAdvise -> IO CInt

foreign import ccall "libltdl.h lt_dladvise_ext"
    c_lt_dladvise_ext :: Ptr DLAdvise -> IO CInt

foreign import ccall "libltdl.h lt_dladvise_global"
    c_lt_dladvise_global :: Ptr DLAdvise -> IO CInt

foreign import ccall "libltdl.h lt_dladvise_local"
    c_lt_dladvise_local :: Ptr DLAdvise -> IO CInt

foreign import ccall "libltdl.h lt_dladvise_resident"
    c_lt_dladvise_resident :: Ptr DLAdvise -> IO CInt

foreign import ccall "libltdl.h lt_dladvise_preload"
    c_lt_dladvise_preload :: Ptr DLAdvise -> IO CInt

foreign import ccall "libltdl.h lt_dlclose"
    c_lt_dlclose :: DLHandle -> IO CInt

foreign import ccall "libltdl.h lt_dlsym"
    c_lt_dlsym :: DLHandle -> CString -> IO (FunPtr a)

foreign import ccall "libltdl.h lt_dlerror"
    c_lt_dlerror :: IO CString

foreign import ccall "libltdl.h lt_dladdsearchdir"
    c_lt_dladdsearchdir :: CString -> IO CInt

foreign import ccall "libltdl.h lt_dlinsertsearchdir"
    c_lt_dlinsertsearchdir :: CString -> CString -> IO CInt

foreign import ccall "libltdl.h lt_dlsetsearchpath"
    c_lt_dlsetsearchpath :: CString -> IO CInt

foreign import ccall "libltdl.h lt_dlgetsearchpath"
    c_lt_dlgetsearchpath :: IO CString

foreign import ccall "libltdl.h lt_dlforeachfile"
    c_lt_dlforeachfile :: CString -> FunPtr (CString -> Ptr a -> IO CInt) -> Ptr a -> IO CInt

foreign import ccall "libltdl.h lt_dlmakeresident"
    c_lt_dlmakeresident :: DLHandle -> IO CInt

foreign import ccall "libltdl.h lt_dlisresident"
    c_lt_dlisresident :: DLHandle -> IO CInt
