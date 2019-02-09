module Bsp
( HP(..)
, Seg(..)
, Csg(..)
, IPt(..)
, intersectHPCsg
, infSeg
, intersectHPs
, rotateHPAround
, rotateHPAroundP
, radialHP
, originHP
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
data HP = HP (V2 Double) (V2 Double) deriving (Eq, Show)

radialHP v = HP v (-(signorm v))
originHP v = HP (V2 0.0 0.0) (signorm v)

rotateHP :: Rotation -> HP -> HP
rotateHP r (HP p d) = HP (rotatePoint r p) (rotatePoint r d)
translateHP :: V2 Double -> HP -> HP
translateHP tv (HP p d) = HP (p + tv) d

rotateHPAround :: Rotation -> HP -> V2 Double -> HP
rotateHPAround r hp center =
  translateHP center (rotateHP r (translateHP (- center) hp))
rotateHPAroundP r hp@(HP p d) = rotateHPAround r hp p

insideHP (HP p d) pt = ((pt - p) `dot` d) > 0

-- A segment is a possibly infinite subset of a line.  The two intersection
-- points are ordered: if the HP were rotated to be the area above the X axies,
-- then the first ipt would be the one on the right.
data Seg = Seg HP (Maybe IPt) (Maybe IPt) | Empty deriving Show
infSeg hp = Seg hp Nothing Nothing

data Csg = Prim HP | Intersection Csg Csg | Union Csg Csg | Difference Csg Csg

intersectHPCsg :: HP -> Csg -> [Seg]
intersectHPCsg hp csg = intersectSegCsg (infSeg hp) csg

intersectSegCsg :: Seg -> Csg -> [Seg]
intersectSegCsg seg (Prim hp) = [intersectSegHP seg hp]
intersectSegCsg seg (Intersection csg0 csg1) =
  pairwiseIntersectSegs (intersectSegCsg seg csg0) (intersectSegCsg seg csg1)

pairwiseIntersectSegs (seg : segs) segs' = (map (intersectSegs seg) segs') ++ (pairwiseIntersectSegs segs segs')
pairwiseIntersectSegs [] _ = []

intersectSegs :: Seg -> Seg -> Seg
intersectSegs Empty _ = Empty
intersectSegs _ Empty = Empty
intersectSegs a@(Seg hp ipt0 ipt1) b =
  assert (segsSameHP a b)
    inter (inter b ipt0) ipt1
    --intersectSegHP (intersectSegHP b (otherHp hp ipt0)) (otherHp hp ipt1)
  where inter seg (Just ipt) = intersectSegHP seg (otherHp hp ipt)
        inter seg Nothing = seg

otherHp hp (IPt hp0 hp1 _)
  | hp == hp0 = hp1
  | hp == hp1 = hp0

segsSameHP (Seg hp _ _) (Seg hp' _ _) = hp == hp'

-- TODO checkSeg: seg's hp must be on of the hps of both intersection points
-- TODO checkSegList: checks that segs are nondecreasing
-- TODO compactSegList: combines adjacent segs

intersectSegHP :: Seg -> HP -> Seg
intersectSegHP Empty hp = Empty
intersectSegHP orig@(Seg segHP ipt0 ipt1) hp
  | ipt0InHP && ipt1InHP = orig
  | not ipt0InHP && ipt1InHP = Seg segHP hpIpt ipt1
  | ipt0InHP && not ipt1InHP = Seg segHP ipt0 hpIpt
  | not ipt0InHP && not ipt1InHP = Empty
  where hpIpt = intersectHPs segHP hp
        ipt0InHP = case ipt0 of (Just (IPt _ _ v)) -> insideHP hp v
                                Nothing -> containsPositiveInf hp segHP
        ipt1InHP = case ipt1 of (Just (IPt _ _ v)) -> insideHP hp v
                                Nothing -> containsNegativeInf hp segHP
        containsPositiveInf (HP _ d) (HP segP segD) = (dot d (- (planePosDir segD))) > 0
        containsNegativeInf (HP _ d) (HP segP segD) = (dot d (planePosDir segD)) > 0
{-
intersectSegHP_ orig@(Seg segHP ipt0@(IPt _ _ ipt0v) ipt1@(IPt _ _ ipt1v)) hp
  | ipt0InHP && ipt1InHP = orig
  | not ipt0InHP && ipt1InHP = Seg segHP hp ipt1InHP
  | ipt0InHP && not ipt1InHP = Seg segHP ipt0InHP hp
  | not ipt0InHP && not ipt1InHP = Empty
  where hpIpt = intersectHPs segHP hp
        ipt0InHP = insideHP hp ipt0v
        ipt1InHP = insideHP hp ipt1v
-}

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
intersectHPsPoint b@(HP p0 d0@(V2 bx by)) c@(HP p1 d1@(V2 cx cy))
  -- | TR.trace (show ("A", ax, ay, ax', ay', cross, b, c)) False = undefined
  | cross == 0.0 = Nothing
  | bx == 0.0 = Just $ V2 ax' ay'
  | otherwise = Just $ V2 ax ay
  where m = p0 `dot` d0
        n = p1 `dot` d1
        cross = ((cy * bx) - (by * cx))
        ay = ((n * bx) - (m * cx)) / cross
        ax = (m - (ay * by)) / bx
        ax' = ((n * by) - (m * cy)) / (- cross)
        ay' = (m - (ax' * bx)) / by
