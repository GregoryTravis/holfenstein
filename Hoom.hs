module Main (main) where

import Control.Concurrent
import Control.Monad (unless)
import Data.Char (ord)
import qualified Data.Set as S
import Linear
import System.Posix.Unistd
import System.IO

import Bsp
import Diag
import Util
import Window

(screenWidth, screenHeight) = (640, 480)

ptToDrawable pt = Dpoint $ toNormalPoint pt

hpToDrawable (HP v _) = DiagT (Dline $ V2 pos neg, Ddiamond v)
  where pos = v + nperp
        neg = v - nperp
        nperp = 2 *^ (planePosDir v)

main = do
  hSetBuffering stdout NoBuffering
  window <- windowInit screenWidth screenHeight

  -- Necessary to make window act normal?
  -- (cursorPos, newKeySet, quitEvent) <- getInput S.empty

  let hp0 = HP (V2 1.0 0.0) True
  let hp1 = HP (V2 0.0 1.0) True
  let pt = intersectHPs hp0 hp1
  msp pt
  msp $ toNormalPoint pt
  msp $ toNormalPoint $ intersectHPs (HP (V2 1.0 1.0) True) (HP (V2 (-1.0) 1.0) True)

  --drawDiag window (Diag [(V2 (V2 3.0 3.0) (V2 5.0 4.0))])
  let dia_ = Diag [a, b, c, d]
             where a = V2 p0 p1
                   b = V2 p1 p2
                   c = V2 p2 p3
                   d = V2 p3 p0
                   p0 = V2 1.0 0.0
                   p1 = V2 0.0 1.0
                   p2 = V2 1.0 2.0
                   p3 = V2 2.0 1.0
  --drawDiag window diag
  let apt = ptToDrawable pt -- Dpoint $ toNormalPoint pt -- Diag $ box (toNormalPoint pt)
  let ahp0 = hpToDrawable hp0
  let ahp1 = hpToDrawable hp1
  let diag = DiagT (apt, Diag [ahp0, ahp1])
  drawDiag window diag

  blit window

  let loop :: KeySet -> IO ()
      loop keySet = do
        (cursorPos, newKeySet, quitEvent) <- getInputWait keySet
        --msp (cursorPos, newKeySet)
        let quit = quitEvent || S.member 27 newKeySet || S.member (ord 'q') newKeySet
        unless quit $ loop newKeySet
        return ()
   in loop S.empty
  --threadDelay 5000000

  windowTerm window
