{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.LibLTDL
import Foreign.Ptr

foreign import ccall "dynamic"
    mkFun :: FunPtr (Double -> Double) -> (Double -> Double)

main :: IO ()
main = do
    dlInit
    h <- dlOpenExt (Just "libm")
    sin_fptr <- dlSym h "sin"
    print $ mkFun sin_fptr (pi/2)
    dlClose h
    dlExit
