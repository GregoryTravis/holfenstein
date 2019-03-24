module Shader
( runShader
) where

import Linear

import Grafix
import Img
import Window

runShader :: Window -> (Int -> Int -> Int -> Int -> Color) -> IO ()
runShader window shader = withFramebuffer window renderPoints
  where (w, h) = getDimensions window
        allPoints = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
        renderPoint ptr pitch (x, y) = drawPoint window (V2 x y) (packColor (shader x y w h)) ptr pitch
        renderPoints ptr pitch = mapM_ (renderPoint ptr pitch) allPoints

