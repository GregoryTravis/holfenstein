module Map where

import qualified Data.Vector as V
import Data.Vector ((!))
import Linear

-- Grid is made of unit cubes
-- Cubes are identified by their least corner coords
-- Walls are identified by their least coord along their axis
-- Remember these are drawn upside down
type Grid = V.Vector (V.Vector Char)

data World = World Grid (Int, Int) deriving Show

outsideGrid :: World -> Int -> Int -> Bool
outsideGrid (World _ (w, h)) x y = x < 0 || y < 0 || x >= w || y >= h
outsideGridF :: World -> V2 Double -> Bool
outsideGridF (World _ (wx, wy)) (V2 x y) = x < 0 || x >= (fromIntegral wx) + 1 || y < -1 || y > (fromIntegral wy) + 1

--isHorWall w x y | TR.trace (show ("isHorWall", x, y)) False = undefined
isHorWall world x y = (isSolid world x (y - 1)) /= (isSolid world x y)
isVerWall world x y = (isSolid world (x - 1) y) /= (isSolid world x y)
isSolid :: World -> Int -> Int -> Bool
--isSolid x y | TR.trace (show ("iS", x, y, (length grid), gridSize, (outsideGrid x y))) False = undefined
isSolid world x y = (getMaterial world x y) /= ' '
getMaterial :: World -> Int -> Int -> Char
--getMaterial w x y | TR.trace (show ("gM", x, y)) False = undefined
getMaterial f@(World grid _) x y
  | outsideGrid f x y = ' '
  | otherwise = ((grid ! x) ! y)

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

