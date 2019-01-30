module Diag
( Diag(..)
, drawDiag
) where

import Linear

import Util
import Window

data Diag = Diag [V2 (V2 Double)] deriving Show

drawDiag :: Window -> Diag -> IO ()
drawDiag window diag = do
  msp diag
  msp $ bbox diag
  return ()

--pointsOf :: Diag -> [(V2 Double)]
--pointsOf (Diag lines) = concat lines

v2p (V2 x y) = (x, y)

bbox :: Diag -> V2 (V2 Double)
bbox (Diag lines) = V2 (V2 minX minY) (V2 maxX maxY)
  where minX = minimum xs
        minY = minimum ys
        maxX = maximum xs
        maxY = maximum ys
        (xs, ys) = unzip tups
        tups = concat (map (\(V2 a b) -> [v2p a, v2p b]) lines)
