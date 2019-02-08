module Main (main) where

import Control.Concurrent
import Control.Monad (unless)
import Data.Char (ord)
import Data.Maybe
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Set as S
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

-- I am giving this a terrible name because I know it must exist but I don't
-- know enough to know what it's called and I refused to accept at the moment
-- that it might be called fmap.
mappily :: (a -> b) -> Maybe a -> Maybe b
mappily f (Just x) = Just (f x)
mappily f Nothing = Nothing

--segToDrawable :: Seg -> DiagT (DiagT Dline Ddiamond) (Diag (Maybe Dpoint))
segToDrawable (Seg hp ipt0 ipt1) = DiagT (hpToDrawable hp, Diag [mappily iptToDrawable ipt0, mappily iptToDrawable ipt1])

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

--animf :: Double -> Diag [(DiagT (DiagT Dline Ddiamond) (Diag (Maybe Dpoint)))]
animf t = DiagT (Diag [hpToDrawable hp, hpToDrawable line], DiagT (Diag [map segToDrawable inter], coordAxes 4.0))
--animf t = hpToDrawable line
  where rot = angToRotation (angVel * t)
        angVel = pi / 4.0
        hp = rotateHPAroundP rot (radialHP (V2 (-1.0) 1.0))
        w = Prim hp
        line = radialHP (V2 0.0 2.0)
        inter = intersectHPCsg line w

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
