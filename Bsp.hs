module Bsp
( HP(..)
--, Seg(..)
--, Segr(..)
, IPt(..)
--, toNormalPoint
, intersectHPs
, planePosDir
, rotateHP
) where

-- TODO: make some of these constructors private?

import qualified Debug.Trace as TR
import Linear

import Math
import Util

-- Half plane: defined by a line and a direction.
-- The line goes through the given point and is perpendicual to the line from
-- the origin to the point.  The point is also called the plane's origin.
-- The direction is encoded as a boolean: does the half plane include the origin?
data HP = HP (V2 Double) Bool deriving Show

rotateHP :: Rotation -> HP -> HP
rotateHP r (HP v b) = HP (rotatePoint r v) b

-- Intersection point: represents the intersection of two planes as both the
-- pair of planes and the point.
data IPt = IPt HP HP (V2 Double) deriving Show

planePosDir (V2 x y) = signorm (V2 (- y) x)

-- The perpendicular distance of all points on a line to the origin is the
-- same, and for two lines, they intersect at the point wihch is the correct
-- perpendicular distance from both lines.  Put another way: a and b are the
-- radial vectors of the two lines.  A point c is on the line if a * a == a *
-- c.  Solve this along with b * b == b * c and you get c.
intersectHPs hp0 hp1 = case intersectHPsPoint hp0 hp1 of Just v -> Just $ IPt hp0 hp1 v
                                                         Nothing -> Nothing
intersectHPsPoint (HP a@(V2 ax ay) _) (HP b@(V2 bx by) _)
  -- | TR.trace (show ("CX", cx, cy, cx', cy', cross, a, b)) False = undefined
  | cross == 0 = Nothing
  | ax == 0 = Just $ V2 cx' cy'
  | otherwise = Just $ V2 cx cy
  where cross = ((ax * by) - (bx * ay))
        cy = ((ax * (dot b b)) - (bx * (dot a a))) / cross
        cx = ((dot a a) - (cy * ay)) / ax
        cx' = ((ay * (dot b b)) - (by * (dot a a))) / (- cross)
        cy' = ((dot a a) - (cx * ax)) / ay
