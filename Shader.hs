module Shader
( runShader
, shaderMain
) where

import Control.Concurrent
import Control.Monad (unless)
import Data.Char (ord)
import Data.Maybe
import qualified Data.Set as S
import Data.Time.Clock.POSIX (getPOSIXTime)
import Linear
import System.Posix.Unistd
import System.IO

import Grafix
import Img
import Window

runShader :: Window -> (Int -> Int -> Int -> Int -> Color) -> IO ()
runShader window shader = withFramebuffer window renderPoints
  where (w, h) = getDimensions window
        allPoints = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
        renderPoint ptr pitch (x, y) = drawPoint window (V2 x y) (packColor (shader x y w h)) ptr pitch
        renderPoints ptr pitch = mapM_ (renderPoint ptr pitch) allPoints

shaderMain screenWidth screenHeight shader = do
  --msp $ implicitCycletoExplicitCycle [(0, 1), (2, 0), (1, 2)]
  window <- windowInit screenWidth screenHeight

  let loop startTime = do
        now <- getPOSIXTime
        let startTime' = case startTime of Nothing -> Just now
                                           Just x -> Just x
        let elapsed = realToFrac $ now - (fromJust startTime')
        --msp elapsed

        runShader window (shader elapsed)
        blit window
        --msp "frame"

        (cursorPos, newKeySet, quitEvent) <- getInput S.empty
        let quit = quitEvent || S.member 27 newKeySet || S.member (ord 'q') newKeySet
        unless quit $ loop startTime'
        return ()
   in loop Nothing

  windowTerm window
