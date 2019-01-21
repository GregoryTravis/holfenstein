{-# LANGUAGE ForeignFunctionInterface #-}
module Gfx where

import Data.Word (Word32)
import Foreign.C
import Foreign.Ptr
 
foreign import ccall "fastestFillVStrip" fastestFillVStrip :: Ptr Word32 -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "sampler" sampler :: CInt -> CInt -> IO CInt
foreign import ccall "fastestTextureVStrip" fastestTextureVStrip :: Ptr Word32 -> Ptr Word32 -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CDouble -> CDouble -> IO ()
