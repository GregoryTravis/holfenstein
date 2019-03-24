module Main (main) where

import Control.Concurrent
import Control.Monad (unless)
import Data.Char (ord)
import Data.Maybe
import qualified Data.Set as S
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Posix.Unistd
import System.IO

import Img
import Math
import Shader
import Util
import Window

(screenWidth, screenHeight) = (640, 480)

shader :: Double -> Int -> Int -> Int -> Int -> Color
shader t x y w h = Color (fade x w) (fade y h) 0
  where fade x m = (floor (256.0 * (((fromIntegral x) / (fromIntegral m)) + t))) `mod` 256

main = do
  --msp $ implicitCycletoExplicitCycle [(0, 1), (2, 0), (1, 2)]
  hSetBuffering stdout NoBuffering
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
