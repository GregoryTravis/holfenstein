module Main (main) where

import System.IO

import Img
import Shader
import Util

(screenWidth, screenHeight) = (640, 480)

type Shader = Double -> Int -> Int -> Int -> Int -> Color
type FShader = Double -> Double -> Double -> Color

shader :: Shader
shader t x y w h = Color (fade x w) (fade y h) 0
  where fade x m = (floor (256.0 * (((fromIntegral x) / (fromIntegral m)) + t))) `mod` 256

fshaderToShader :: FShader -> Shader
fshaderToShader fs t x y w h = fs t fx fy
  where minDim = min w h
        fx = (fromIntegral (x - (w `div` 2))) / (fromIntegral (minDim `div` 2))
        fy = - ((fromIntegral (y - (h `div` 2))) / (fromIntegral (minDim `div` 2)))

fshader :: FShader
fshader t x y = --Color r 0 0
  if y > 0.5 then Color 255 0 0 else Color 0 255 0
  where r = clip $ x * 256.0
        clip x | x < 0.0 = 0
               | x >= 256.0 = 255
               | otherwise = floor x

main = do
  hSetBuffering stdout NoBuffering
  shaderMain screenWidth screenHeight (fshaderToShader fshader)
