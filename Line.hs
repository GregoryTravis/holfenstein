module Line
( Line(..)
, boxAround
, scaleLines
, translateLines
) where

import Linear

data Line a = Line (V2 a) (V2 a) deriving Show

scale :: Num a => V2 a -> Line a -> Line a
scale s (Line a b) = Line (s * a) (s * b)
translate v (Line a b) = Line (v + a) (v + b)
scaleLines s lines = map (scale s) lines
translateLines v lines = map (translate v) lines

box :: V2 a -> V2 a -> [Line a]
box (V2 x0 y0) (V2 x1 y1) = [Line a b, Line b c, Line c d, Line d a]
  where a = V2 x0 y0
        b = V2 x1 y0
        c = V2 x1 y1
        d = V2 x0 y1

boxAround :: V2 Double -> [Line Double]
boxAround center = box (center - boxOffset) (center + boxOffset)
  where boxOffset = V2 0.05 0.05
