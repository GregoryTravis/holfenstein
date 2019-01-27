module Map where

import Linear

-- World is made of unit cubes
-- Cubes are identified by their least corner coords
-- Walls are identified by their least coord along their axis
-- Remember these are drawn upside down
type World = [[Char]]

data Frab = Frab World (Int, Int) deriving Show

outsideWorld :: Frab -> Int -> Int -> Bool
outsideWorld (Frab _ (w, h)) x y = x < 0 || y < 0 || x >= w || y >= h
outsideWorldF :: Frab -> V2 Double -> Bool
outsideWorldF (Frab _ (wx, wy)) (V2 x y) = x < 0 || x >= (fromIntegral wx) + 1 || y < -1 || y > (fromIntegral wy) + 1

--isHorWall w x y | TR.trace (show ("isHorWall", x, y)) False = undefined
isHorWall frab x y = (isSolid frab x (y - 1)) /= (isSolid frab x y)
isVerWall frab x y = (isSolid frab (x - 1) y) /= (isSolid frab x y)
isSolid :: Frab -> Int -> Int -> Bool
--isSolid x y | TR.trace (show ("iS", x, y, (length world), worldSize, (outsideWorld x y))) False = undefined
isSolid frab x y = (getMaterial frab x y) /= ' '
getMaterial :: Frab -> Int -> Int -> Char
--getMaterial w x y | TR.trace (show ("gM", x, y)) False = undefined
getMaterial f@(Frab world _) x y
  | outsideWorld f x y = ' '
  | otherwise = ((world !! x) !! y)

data WallPt = Ver Int Double | Hor Double Int deriving (Eq, Show)
wallPtToV2 (Ver x y) = V2 (fromIntegral x) y
wallPtToV2 (Hor x y) = V2 x (fromIntegral y)
wallPtInt (Ver x y) = (x, floor y)
wallPtInt (Hor x y) = (floor x, y)
sameWall (Hor fx0 y0) (Hor fx1 y1) = y0 == y1 && ((floor fx0) == (floor fx1))
sameWall (Ver x0 fy0) (Ver x1 fy1) = x0 == x1 && ((floor fy0) == (floor fy1))
sameWall _ _ = False
transposeHit (Hor x y) = Ver y x
transposeHit (Ver x y) = Hor y x
transposeMaybeHit (Just hit) = Just $ transposeHit hit
transposeMaybeHit Nothing = Nothing
-- x postition of hit relative to wall origin
horPos (Hor x y) = case properFraction x of (_, hp) -> hp
horPos (Ver x y) = case properFraction y of (_, hp) -> hp

