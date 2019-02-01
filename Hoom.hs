module Main (main) where

import Control.Concurrent
import Control.Monad (unless)
import qualified Data.Set as S
import Linear
import System.Posix.Unistd
import System.IO

import Diag
import Util
import Window

(screenWidth, screenHeight) = (640, 480)

main = do
  hSetBuffering stdout NoBuffering
  window <- windowInit screenWidth screenHeight

  -- Necessary to make window act normal?
  -- (cursorPos, newKeySet, quitEvent) <- getInput S.empty

  drawDiag window (Diag [(V2 (V2 3.0 3.0) (V2 5.0 4.0))])

  blit window

  let loop :: KeySet -> IO ()
      loop keySet = do
        (cursorPos, newKeySet, quitEvent) <- getInputWait keySet
        --msp (cursorPos, newKeySet)
        let quit = quitEvent || S.member 27 newKeySet
        unless quit $ loop newKeySet
        return ()
   in loop S.empty
  --threadDelay 5000000

  windowTerm window
