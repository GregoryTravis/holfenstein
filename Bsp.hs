module Bsp
( HP(..)
, Seg(..)
, Segr(..)
, Pt(..)
, toNormalPoint
, intersectHPs
) where

-- TODO: make some of these constructors private?

import Linear


-- Half plane: defined by a line and a direction.
-- The line goes through the given point and is perpendicual to the line from
-- the origin to the point.  The point is also called the plane's origin.
-- The direction is encoded as a boolean: does the half plane include the origin?
data HP = HP (V2 Double) Bool deriving Show

-- Segment: a line segment, line, ray, or empty.
-- Defined by a HP and a distance from the HP's point along the HP's line.
-- The positive direction along the HP's line is the one going CCW.
-- TODO: consider using the square of the distance.
data Seg = Seg HP Segr deriving Show
data Segr = Inf | PosInf Double | NegInf Double | Fin Double Double | Empty deriving Show

infSeg :: HP -> Seg
infSeg hp = Seg hp Inf

-- Intersection point: represents the intersection of two planes as one of the
-- planes and the signed distance from the origin.  For HP at (1, 0), the
-- positive direction along the plane's line is up.
-- TODO: is there a more symmetrical way of doing this?
-- TODO: should we just represent the point as two planes?
data Pt = Pt HP Double deriving Show

planePosDir (V2 x y) = signorm (V2 (- y) x)

toNormalPoint (Pt (HP po _) dist) = po + (dist *^ (planePosDir po))

-- The perpendicular distance of all points on a line to the origin is the
-- same, and for two lines, they intersect at the point wihch is the correct
-- perpendicular distance from both lines.  Put another way: a and b are the
-- radial vectors of the two lines.  A point c is on the line if a * a == a *
-- c.  Solve this along with b * b == b * c and you get c.
intersectHPs (HP (V2 ax ay) _) b@(HP bp@(V2 bx by) _) = Pt b dist
  where cy = ((ax * bx * bx) + (ax * by * by) - (bx * ax *ax) - (bx * ay * ay)) / ((ax * by) - (bx * ay))
        cx = ((ax * ax) + (ay * ay) - (ay * cy)) / ax
        c = V2 cx cy
        --dist = sqrt (dot c (c-b))
        dist = (c - bp) `dot` (planePosDir bp)
