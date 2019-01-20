{-# LANGUAGE ForeignFunctionInterface #-}
module Gfx where

import Data.Word (Word32)
import Foreign.C
import Foreign.Ptr
 
foreign import ccall "fastestDrawVStrip" fastestDrawVStrip :: Ptr Word32 -> CInt -> CInt -> CInt -> IO ()
