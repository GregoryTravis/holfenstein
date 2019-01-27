module Img
( Color(..)
, Img(..)
, PackedColor
, packColor
, pixAt
, readImg
, darkenImg
, white
, lightGray
, darkGray
) where

import Codec.Picture
import Data.Bits
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Word (Word32)

data Color = Color Int Int Int deriving Show

type PackedColor = Word32
white = packColor $ Color 255 255 255
lightGray = packColor $ Color 200 200 200
darkGray = packColor $ Color 120 120 120

packColor :: Color -> Word32
packColor (Color r g b) =
  fromIntegral $ (shift r 24) .|. (shift g 16) .|. (shift b 8) .|. 0xff

unpackPixel (PixelRGBA8 r g b a) = Color (fromIntegral r) (fromIntegral g) (fromIntegral b)

unpackColor :: Word32 -> Color
unpackColor w = (Color r g b)
  where r = (shiftR r 24) .&. 0xff
        g = (shiftR r 16) .&. 0xff
        b = (shiftR r 8) .&. 0xff

type Vec2D a = V.Vector (V.Vector a)
data Img = Img (Vec2D Color) Int Int

pixAt (Img colors w h) x y = (colors ! y) ! x

readImg :: String -> IO Img
readImg fileName = do
  res <- readImage fileName
  let image = case res of (Right image) -> convertRGBA8 $ image
  let Image { imageWidth = w, imageHeight = h } = image
  let colors = V.fromList (map V.fromList [[unpackPixel $ pixelAt image x y | x <- [0..w-1]] | y <- [0..h-1]])
  return $ Img colors w h

mapImg :: (Color -> Color) -> Img -> Img
mapImg f (Img colors w h) = Img ncolors w h
  where colorsL = map V.toList (V.toList colors)
        ncolorsL = map (map f) colorsL
        ncolors = V.fromList (map V.fromList ncolorsL)

darkenImg :: Img -> Img
darkenImg = (mapImg darken)
  where darken (Color r g b) = Color (r `div` 2) (g `div` 2) (b `div` 2) 
