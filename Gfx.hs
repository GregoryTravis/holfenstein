{-# LANGUAGE ForeignFunctionInterface #-}
module Gfx
( drawLine
, drawPoint
, fastestTextureVStrip
, floorAndCeiling
) where

import Control.Monad (when, unless)
import Data.Word (Word32)
import qualified Debug.Trace as TR
import Foreign.C
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr
import Foreign.Storable (pokeElemOff)
import Linear

import Img
import Line
import Util
import Window
 
foreign import ccall "fastestTextureVStrip" fastestTextureVStrip :: Ptr Word32 -> Ptr Word32 -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CDouble -> CDouble -> IO ()

toOffset (V2 x y) pitch = y * (pitch `div` 4) + x

inScreenBounds w (V2 x y) = case getDimensions w of (w, h) -> x >= 0 && y >= 0 && x < w && y < h

drawPoint :: Window -> V2 Int -> PackedColor -> Ptr Word32 -> Int -> IO ()
drawPoint w v c ptr pitch = do
  assertM (v, ptr, pitch) (inScreenBounds w v)
    pokeElemOff ptr (toOffset v pitch) c
drawPoint_ w v c ptr pitch = do
  when (inScreenBounds w v) $ pokeElemOff ptr (toOffset v pitch) c

drawLine :: Window -> Line Int -> PackedColor -> Ptr Word32 -> Int -> IO ()
drawLine w (Line a@(V2 x0 y0) (V2 x1 y1)) color ptr pitch = step fa delta count
  where delta | isVert = V2 ((signum dx) * (abs (dx / dy))) (signum dy)
              | otherwise = V2 (signum dx) ((signum dy) * (abs (dy / dx)))
        count | isVert = abs idy
              | otherwise = abs idx
        fa :: V2 Double
        fa = V2 (fromIntegral x0) (fromIntegral y0)
        isVert = (abs dy) > (abs dx)
        idx = x1 - x0
        idy = y1 - y0
        dx :: Double
        dx = fromIntegral idx
        dy :: Double
        dy = fromIntegral idy
        step :: V2 Double -> V2 Double -> Int -> IO ()
        step a@(V2 x y) delta count
          -- | TR.trace (show ("step", a)) False = undefined
          | count == 0 = return ()
          | otherwise = do
              drawPoint w (V2 (floor x) (floor y)) color ptr pitch
              step (a + delta) delta (count - 1)

floorAndCeiling :: Window -> Ptr Word32 -> Int -> IO ()
floorAndCeiling window wordPtr pitch = do
  fillBytes wordPtr (fromIntegral 84) half
  fillBytes (plusPtr wordPtr half) (fromIntegral 40) half
  return ()
  where half = (pitch * screenHeight) `div` 2
        (_, screenHeight) = getDimensions window

