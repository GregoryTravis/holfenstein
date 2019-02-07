module Bsp
( HP(..)
--, Seg(..)
--, Segr(..)
, IPt(..)
--, toNormalPoint
, intersectHPs
, planePosDir
, rotateHP
, translateHP
) where

-- TODO: make some of these constructors private?

import qualified Debug.Trace as TR
import Linear

import Math
import Util

-- Half plane: defined by a point and a normalized direction.  The line
-- bounding the half plane is perpendicular to the direction vector, and the
-- direction vector points into the half plane bounded by the line.
data HP = HP (V2 Double) (V2 Double) deriving Show

rotateHP :: Rotation -> HP -> HP
rotateHP r (HP p d) = HP (rotatePoint r p) (rotatePoint r d)
translateHP :: V2 Double -> HP -> HP
translateHP tv (HP p d) = HP (p + tv) d

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
intersectHPsPoint a@(HP p0 d0@(V2 bx by)) b@(HP p1 d1@(V2 cx cy))
  -- | TR.trace (show ("A", ax, ay, ax', ay', a, b)) False = undefined
  | cross == 0.0 = Nothing
  | bx == 0.0 = Just $ V2 ax' ay'
  | otherwise = Just $ V2 ax ay
  where m = p0 `dot` d0
        n = p1 `dot` d1
        cross = ((cy * bx) - (by * cx))
        ay = ((n * bx) - (m * cx)) / cross
        ax = (m - (ay * by)) / bx
        ax' = ((n * by) - (m * cy)) / (- cross)
        ay' = (m - (ax * bx)) / by
