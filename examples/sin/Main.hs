{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.LibLTDL
import Foreign.Ptr

foreign import ccall "dynamic"
    mkFun :: FunPtr (Double -> Double) -> (Double -> Double)

main :: IO ()
main = withDL Nothing $ \h -> do
    sin_fptr <- dlSym h "sin"
    print $ mkFun sin_fptr (pi/2)
