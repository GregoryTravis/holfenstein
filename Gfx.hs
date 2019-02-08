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

doSafeDrawPoint = False
drawPoint :: Window -> V2 Int -> PackedColor -> Ptr Word32 -> Int -> IO ()
assertDrawPoint w v c ptr pitch = do
  assertM (v, ptr, pitch) (inScreenBounds w v)
    pokeElemOff ptr (toOffset v pitch) c
safeDrawPoint w v c ptr pitch = do
  when (inScreenBounds w v) $ pokeElemOff ptr (toOffset v pitch) c
drawPoint = if doSafeDrawPoint then safeDrawPoint else assertDrawPoint

-- TODO this really should take doubles
-- Can get rid of these three params, but nicely?
drawLine :: Window -> Line Int -> PackedColor -> Ptr Word32 -> Int -> IO ()
drawLine w line color ptr pitch = case clipLine line dim of Nothing -> return ()
                                                            Just line -> drawLineUnclipped w line color ptr pitch
  where dim = getDimensions w

clipLine :: Line Int -> (Int, Int) -> Maybe (Line Int)
clipLine line (winW, winH) = case clipLineHor line winW of Just newLine -> clipLineVer newLine winH
                                                           Nothing -> Nothing

clipLineHor :: Line Int -> Int -> Maybe (Line Int)
clipLineHor line winW = case clipLineLowX line winW of Just newLine -> clipLineHighX newLine winW
                                                       Nothing -> Nothing
clipLineVer :: Line Int -> Int -> Maybe (Line Int)
clipLineVer line winH = case clipLineHor (xyFlipLine line) winH of Just newLine -> Just $ xyFlipLine newLine
                                                                   Nothing -> Nothing

xyFlipLine (Line (V2 x0 y0) (V2 x1 y1)) = Line (V2 y0 x0) (V2 y1 x1)

-- Clips line to 0
clipLineLowX :: Line Int -> Int -> Maybe (Line Int)
clipLineLowX l@(Line a@(V2 x0 y0) b@(V2 x1 _)) winW
  | x0 > x1 = clipLineLowX (Line b a) winW
  | x0 < 0 && x1 < 0 = Nothing
  | x0 < 0 && x1 >= 0 = Just clipped
  | otherwise = Just l
  where clipped = Line clippedPt b
        clippedPt = V2 0 newY
        newY = floor $ (fromIntegral y0) + (m * (fromIntegral (-x0)))
        m = case b - a of (V2 deltaX deltaY) -> (fromIntegral deltaY) / (fromIntegral deltaX)
clipLineHighX :: Line Int -> Int -> Maybe (Line Int)
clipLineHighX line winW = case clipLineLowX (flipLine line winW) winW of Just newLine -> Just $ flipLine newLine winW
                                                                         Nothing -> Nothing

flipLine (Line a b) winW = Line (flip a) (flip b)
  where flip (V2 x y) = V2 (winW - x - 1) y

drawLineUnclipped :: Window -> Line Int -> PackedColor -> Ptr Word32 -> Int -> IO ()
drawLineUnclipped w (Line a@(V2 x0 y0) (V2 x1 y1)) color ptr pitch = step fa delta count
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

