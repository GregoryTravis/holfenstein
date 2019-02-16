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
import Img
import Math
import Util
import Window

(screenWidth, screenHeight) = (640, 480)

--hpToDrawable :: HP -> DiagT Dline Ddiamond
hpToDrawable (HP p d) = DiagT (Dline darkGray $ V2 pos neg, Ddiamond darkGray p)
  where pos = p + (0.7 * (planePosDir d))
        neg = p - (0.7 * (planePosDir d))

segToDrawable (Seg hp@(HP p d) php nhp) = DiagT (Dline white (V2 a b), Ddiamond white p)
  where a = case php of Just hp' -> case intersectHPs hp hp' of Just v -> v
                                                                Nothing -> p - 1000 * (planePosDir d)
                        Nothing -> p - 1000 * (planePosDir d)
        b = case nhp of Just hp' -> case intersectHPs hp hp' of Just v -> v
                                                                Nothing -> p + 1000 * (planePosDir d)
                        Nothing -> p + 1000 * (planePosDir d)
segToDrawable (Empty (HP p d)) = DiagT (Dline white (V2 a b), Ddiamond white p)
  -- I think 0.1 just results in a 0-length line but that's fine
  where a = p - (0.1 * planePosDir d)
        b = p + (0.1 * planePosDir d)

csgToDrawable :: Csg -> Diag (DiagT Dline Ddiamond)
csgToDrawable csg = Diag $ map hpToDrawable $ gatherLines csg

allCombinations [] = []
allCombinations (x : xs) = [(x, y) | y <- xs] ++ (allCombinations xs)

--square = [HP r l, HP u d, HP l r, HP d u]
square n = map radialHP [r, u, l, d]
  where r = V2 n 0.0
        l = V2 (-n) 0.0
        u = V2 0.0 n
        d = V2 0.0 (- n)
diamond n = [radialHP (V2 (-n) n),
             radialHP (V2 (n) n),
             radialHP (V2 (n) (-n)),
             radialHP (V2 (-n) (-n))]
allIntersections hps = catMaybes $ map intr (allCombinations hps)
  where intr (a, b) = intersectHPs a b

coordAxes scale = Diag [Dline darkGray (V2 x0 x1), Dline darkGray (V2 y0 y1)]
  where x0 = V2 (- (scale / 20.0)) 0.0
        x1 = V2 scale 0.0
        y0 = V2 0.0 (- (scale / 20.0))
        y1 = V2 0.0 scale

animf t = DiagT (csgToDrawable csg, DiagT (Diag [map segToDrawable (concat inters)], coordAxes 4.0))
  where rot = angToRotation ang
        ang = (angVel * t)
        -- ang = 0 -- (angVel * t)
        angVel = pi / 8.0

        csgu = Intersection (convex (square 1.5)) (translateCsg (V2 1.4 0.0) (convex (diamond 1.0)))
        --csgu = convex hps
        --csgu = convex (square 1.0)
        csg = rotateCsgAround rot (V2 (-1.0) 1.0) csgu
        inters = map (\line -> intersectHPCsg line csg) (gatherLines csg)
        --line = radialHP (V2 0.0 0.5)
        --inters = esp $ map (\line -> intersectHPCsg line csg) [line]

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
