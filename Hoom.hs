module Main (main) where

import Control.Concurrent
import Control.Monad (unless)
import Data.Char (ord)
import Data.Maybe
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Set as S
import qualified Debug.Trace as TR
import Linear
import System.Posix.Unistd
import System.IO

import Anim
import Bsp
import Diag
import Math
import Util
import Window

(screenWidth, screenHeight) = (640, 480)

--iptToDrawable :: IPt -> Dpoint
iptToDrawable (IPt _ _ v) = Dpoint v

--hpToDrawable :: HP -> DiagT Dline Ddiamond
hpToDrawable (HP p d) = DiagT (Dline $ V2 pos neg, Ddiamond p)
  where pos = p + (planePosDir d)
        neg = p - (planePosDir d)

segToDrawable (Seg (HP p d) ipt0 ipt1) = DiagT (Dline (V2 a b), Ddiamond p)
  where a = case ipt0 of Just (IPt _ _ v) -> v
                         Nothing -> p - 1000 * (planePosDir d)
        b = case ipt1 of Just (IPt _ _ v) -> v
                         Nothing -> p + 1000 * (planePosDir d)
segToDrawable (Empty (HP p d)) = DiagT (Dline (V2 a b), Ddiamond p)
  -- I think 0.1 just results in a 0-length line but that's fine
  where a = p - (0.1 * planePosDir d)
        b = p + (0.1 * planePosDir d)

csgToDrawable :: Csg -> Diag (DiagT Dline Ddiamond)
csgToDrawable csg = Diag $ map hpToDrawable $ gatherLines csg
gatherLines (Prim hp) = [hp]
gatherLines (Intersection a b) = (gatherLines a) ++ (gatherLines b)
gatherLines (Union a b) = (gatherLines a) ++ (gatherLines b)
gatherLines (Difference a b) = (gatherLines a) ++ (gatherLines b)

allCombinations [] = []
allCombinations (x : xs) = [(x, y) | y <- xs] ++ (allCombinations xs)

square = [HP r l, HP u d, HP l r, HP d u]
  where r = V2 1.0 0.0
        l = V2 (-1.0) 0.0
        u = V2 0.0 1.0
        d = V2 0.0 (- 1.0)
allIntersections hps = catMaybes $ map intr (allCombinations hps)
  where intr (a, b) = intersectHPs a b

animf_ t = DiagT (Diag drhps, Diag drpts)
  where rot = angToRotation (angVel * t)
        angVel = pi / 4.0
        rhps = map (rotateHP rot) hps
        hps = square
        drhps = map hpToDrawable rhps
        rpts = allIntersections rhps
        drpts = map iptToDrawable rpts

coordAxes scale = Diag [Dline (V2 x0 x1), Dline (V2 y0 y1)]
  where x0 = V2 (- (scale / 20.0)) 0.0
        x1 = V2 scale 0.0
        y0 = V2 0.0 (- (scale / 20.0))
        y1 = V2 0.0 scale

animf t = DiagT (csgToDrawable csg, DiagT (Diag [map segToDrawable inter], coordAxes 4.0))
  where rot = angToRotation (angVel * t)
        angVel = pi -- / 4.0

        csg = rotateCsgAround rot (V2 (-1.0) 1.0) w
        w = convex [radialHP (V2 (-1.0) 1.0),
                    radialHP (V2 (1.0) 1.0),
                    radialHP (V2 (1.0) (-1.0)),
                    radialHP (V2 (-1.0) (-1.0))]

        line = radialHP (V2 0.0 2.0)
        inter = intersectHPCsg line csg

main = do
  hSetBuffering stdout NoBuffering
  window <- windowInit screenWidth screenHeight

  -- Necessary to make window act normal?
  -- (cursorPos, newKeySet, quitEvent) <- getInput S.empty

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
{-
  let animf t = DiagT (apt, Diag [ahp0, ahp1])
        where apt = ptToDrawable pt
              ahp0 = hpToDrawable rhp0
              ahp1 = hpToDrawable rhp1
              hp0 = HP (V2 (- 1.0) 0.0) True
              hp1 = HP (V2 ( 1.0) 0.0) True
              rhp0 = rotateHP rot hp0
              rhp1 = rotateHP rot hp1
              pt = intersectHPs rhp0 rhp1
              rot = angToRotation (angVel * t)
              angVel = pi / 4.0
-}
  let anim = Anim animf

  let loop startTime keySet = do
        now <- getPOSIXTime
        let startTime' = case startTime of Nothing -> Just now
                                           Just x -> Just x
        let elapsed = realToFrac $ now - (fromJust startTime')
        --msp elapsed

        drawDiag window (frameAt anim elapsed)
        blit window
        --msp "frame"

        (cursorPos, newKeySet, quitEvent) <- getInput keySet
        --msp (cursorPos, newKeySet)
        let quit = quitEvent || S.member 27 newKeySet || S.member (ord 'q') newKeySet
        unless quit $ loop startTime' newKeySet
        return ()
   in loop Nothing S.empty
  --threadDelay 5000000

  windowTerm window
