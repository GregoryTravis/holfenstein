module Main (main) where

import Control.Concurrent
import Linear
import System.Posix.Unistd
import System.IO

import Diag
import Window

(screenWidth, screenHeight) = (640, 480)

main = do
  hSetBuffering stdout NoBuffering
  window <- windowInit screenWidth screenHeight

  drawDiag window (Diag [(V2 (V2 3.0 3.0) (V2 5.0 4.0))])

  blit window

  --threadDelay 5000000

  windowTerm window
