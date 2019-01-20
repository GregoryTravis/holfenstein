{-# LANGUAGE ForeignFunctionInterface #-}
module Gfx where

import Data.Word (Word32)
import Foreign.C
import Foreign.Ptr
 
foreign import ccall "gfx" gfx :: Ptr Word32 -> CInt -> IO ()
foreign import ccall "gfx2" gfx2 :: Ptr Word32 -> CInt -> IO (Ptr Word32)
foreign import ccall "fastestDrawVStrip" fastestDrawVStrip :: Ptr Word32 -> CInt -> CInt -> CInt -> IO ()
