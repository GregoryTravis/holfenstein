module Bsp
( HP(..)
, Seg(..)
, Csg(..)
, intersectHPCsg
, infSeg
, intersectHPs
, rotateHPAround
, rotateHPAroundP
, rotateCsg
, translateCsg
, rotateCsgAround
, gatherLines
, convex
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

rotateHPAround :: Rotation -> V2 Double -> HP -> HP
rotateHPAround r center hp =
  translateHP center (rotateHP r (translateHP (- center) hp))
rotateHPAroundP r hp@(HP p d) = rotateHPAround r p hp

insideHP (HP p d) pt = ((pt - p) `dot` d) > 0
negateHP (HP p d) = HP p (-d)

-- A segment is a possibly infinite subset of a line, with a distinguished
-- side.  The two intersection points are ordered: if the HP were rotated to be
-- the area above the X axis, then the first hp would be the one on the right,
-- and thus the more positive one in the local line space of the base hp.
data Seg = Seg HP (Maybe HP) (Maybe HP) | Empty HP deriving Show
infSeg hp = Seg hp Nothing Nothing

data Csg = Prim HP | Intersection Csg Csg | Union Csg Csg | Difference Csg Csg

applyCsg :: (HP -> HP) -> Csg -> Csg
applyCsg f (Prim hp) = Prim (f hp)
applyCsg f (Intersection a b) = Intersection (applyCsg f a) (applyCsg f b)
applyCsg f (Union a b) = Union (applyCsg f a) (applyCsg f b)
applyCsg f (Difference a b) = Difference (applyCsg f a) (applyCsg f b)
rotateCsg r = applyCsg $ rotateHP r
translateCsg r = applyCsg $ translateHP r
rotateCsgAround r center csg =
  translateCsg center (rotateCsg r (translateCsg (- center) csg))

convex :: [HP] -> Csg
convex [hp] = Prim hp
convex (hp : hps) = Intersection (Prim hp) (convex hps)

gatherLines (Prim hp) = [hp]
gatherLines (Intersection a b) = (gatherLines a) ++ (gatherLines b)
gatherLines (Union a b) = (gatherLines a) ++ (gatherLines b)
gatherLines (Difference a b) = (gatherLines a) ++ (gatherLines b)

intersectHPCsg :: HP -> Csg -> [Seg]
intersectHPCsg hp csg = intersectSegCsg (infSeg hp) csg

intersectSegCsg :: Seg -> Csg -> [Seg]
intersectSegCsg seg (Prim hp)
  -- Don't self-clip
  | segsSameSegHP seg hp = [seg]
  | otherwise = [intersectSegHP seg hp]
intersectSegCsg seg (Intersection csg0 csg1) =
  pairwiseIntersectSegs (intersectSegCsg seg csg0) (intersectSegCsg seg csg1)
intersectSegCsg seg (Union csg0 csg1) =
  (intersectSegCsg seg csg0) ++ (intersectSegCsg seg csg1)

pairwiseIntersectSegs (seg : segs) segs' = (map (intersectSegs seg) segs') ++ (pairwiseIntersectSegs segs segs')
pairwiseIntersectSegs [] _ = []

intersectSegs :: Seg -> Seg -> Seg
intersectSegs e@(Empty _) _ = e
intersectSegs _ e@(Empty _) = e
intersectSegs a@(Seg hp php nhp) b =
  assert (segsSameHP a b)
    inter (inter b php) nhp
  where inter seg (Just hp) = intersectSegHP seg hp
        inter seg Nothing = seg

segsSameHP (Seg hp _ _) (Seg hp' _ _) = hp == hp'
segsSameSegHP :: Seg -> HP -> Bool
segsSameSegHP (Seg hp _ _) hp' = hp == hp'

-- TODO checkSeg: seg's hp must be on of the hps of both intersection points
-- TODO checkSegList: checks that segs are nondecreasing
-- TODO compactSegList: combines adjacent segs

intersectSegHP seg hp =
  let res = intersectSegHP' seg hp
   in res -- eesp (show ("IHP", res, seg, hp)) res
intersectSegHP' :: Seg -> HP -> Seg
intersectSegHP' e@(Empty _) hp = e
intersectSegHP' orig@(Seg segHP php nhp) hp
  | phpInHP && nhpInHP = orig
  | not phpInHP && nhpInHP = Seg segHP (Just hp) nhp
  | phpInHP && not nhpInHP = Seg segHP php (Just hp)
  | not phpInHP && not nhpInHP = Empty segHP
  where phpInHP = intersectionInside segHP php hp
        nhpInHP = intersectionInside (negateHP segHP) nhp hp

intersectionInside :: HP -> Maybe HP -> HP -> Bool
intersectionInside segHP Nothing hp = posOfHPInsideHP segHP hp
intersectionInside segHP (Just php) hp = case intersectHPs segHP php of Just p -> insideHP hp p
                                                           Nothing -> posOfHPInsideHP segHP hp

posOfHPInsideHP :: HP -> HP -> Bool
posOfHPInsideHP (HP p d) (HP p' d')
  -- TODO This is more correct
  -- | abs(d_perp `dot` d') < 0.001 = ((p - p) `dot` d') > 0.0
  | d_perp `dot` d' == 0 = ((p - p') `dot` d') > 0.0
  | otherwise = (d_perp `dot` d') < 0.0
  where d_perp = planePosDir d

-- When checking if a segment endpoint is inside a HP, we have to handle the case where there is no
-- endpoint.  In this case we should use a point-at-infinity, but since I don't quite know how to
-- do that, we just use a point far, far along the segment.
farFarAway = 1000.0
effectiveIntersectionPos :: Seg -> V2 Double
effectiveIntersectionPos (Seg hp (Just php) nhp) = case intersectHPs hp php of Just v -> v
                                                                               Nothing -> effectiveIntersectionPos (Seg hp Nothing nhp)
effectiveIntersectionPos (Seg (HP p d) Nothing _) = p - farFarAway * (planePosDir d)
effectiveIntersectionNeg (Seg hp php (Just nhp)) = case intersectHPs hp nhp of Just v -> v
                                                                               Nothing -> effectiveIntersectionNeg (Seg hp php Nothing)
effectiveIntersectionNeg (Seg (HP p d) _ Nothing) = p + farFarAway * (planePosDir d)

planePosDir (V2 x y) = signorm (V2 (- y) x)

-- The perpendicular distance of all points on a line to the origin is the
-- same, and for two lines, they intersect at the point wihch is the correct
-- perpendicular distance from both lines.  Put another way: a and b are the
-- radial vectors of the two lines.  A point c is on the line if a * a == a *
-- c.  Solve this along with b * b == b * c and you get c.
intersectHPs b@(HP p0 d0@(V2 bx by)) c@(HP p1 d1@(V2 cx cy))
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
