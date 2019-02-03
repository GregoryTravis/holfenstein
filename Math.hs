module Math where

import Linear

data Rotation = Rotation (V2 (V2 Double)) deriving Show

-- pair of rows
angToRotation ang = V2 (V2 rx (- ry)) (V2 ry rx)
  where rx = cos ang
        ry = sin ang

rotatePoint :: Rotation -> V2 Double -> V2 Double
rotatePoint (Rotation (V2 (V2 a b) (V2 c d))) (V2 x y) = V2 (a * x + b * y) (c * x + d * y)

floorV (V2 x y) = V2 (floor x) (floor y)
