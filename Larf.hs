module Main (main) where

import System.IO

import Img
import Shader
import Util

(screenWidth, screenHeight) = (640, 480)

shader :: Double -> Int -> Int -> Int -> Int -> Color
shader t x y w h = Color (fade x w) (fade y h) 0
  where fade x m = (floor (256.0 * (((fromIntegral x) / (fromIntegral m)) + t))) `mod` 256

main = do
  hSetBuffering stdout NoBuffering
  shaderMain screenWidth screenHeight shader
