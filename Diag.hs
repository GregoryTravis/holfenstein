module Diag
( Diag(..)
, drawDiag
) where

import Data.Word (Word32)
import Foreign.Ptr
import Linear

import Gfx
import Img
import Line
import Math
import Util
import Window

data Diag = Diag [V2 (V2 Double)] deriving Show

drawDiag :: Window -> Diag -> IO ()
drawDiag window diag = do
  --msp diag
  --msp $ bbox diag
  --msp $ boxToBoxTransform (V2 (V2 2.0 2.0) (V2 3.0 3.0)) (V2 (V2 1.0 1.0) (V2 3.0 3.0))
  withFramebuffer window foo
  return ()
  where foo :: Ptr Word32 -> Int -> IO ()
        foo ptr pitch = mapM_ (\line -> drawLine window line white ptr pitch) (esp (toLines (transformDiag winT diag)))
        winT = boxToBoxTransform (bbox diag) (V2 (V2 0.0 0.0) winV)
        winV = case (getDimensions window) of (w, h) -> V2 (fromIntegral (w - 1)) (fromIntegral (h - 1))
        toLines (Diag lines) = map toLine lines
        toLine (V2 a b) = Line (floorV a) (floorV b)

--pointsOf :: Diag -> [(V2 Double)]
--pointsOf (Diag lines) = concat lines

v2p (V2 x y) = (x, y)

-- scale, translate
data Trans = Trans Double (V2 Double) deriving Show

composeTrans (Trans s0 t0) (Trans s1 t1) = Trans (s0 * s1) ((s1 *^ t0) + t1)

transformDiag t (Diag lines) = Diag (map (transformLine t) lines)
transformLine :: Trans -> V2 (V2 Double) -> V2 (V2 Double)
transformLine t (V2 a b) = V2 (transformV t a) (transformV t b)
transformV (Trans s t) v = (s *^ v) + t

boxToBoxTransform :: V2 (V2 Double) -> V2 (V2 Double) -> Trans
boxToBoxTransform (V2 ll0 ur0) (V2 ll1 ur1) =
  composeTrans (Trans 1 (- ll0)) (composeTrans (Trans rescale (V2 0 0)) (Trans 1 ll1))
  where rescale = min xscale yscale
        (V2 w0 h0) = ur0 - ll0
        (V2 w1 h1) = ur1 - ll1
        xscale = w1 / w0
        yscale = h1 / h0

bbox :: Diag -> V2 (V2 Double)
bbox (Diag lines) = V2 (V2 minX minY) (V2 maxX maxY)
  where minX = minimum xs
        minY = minimum ys
        maxX = maximum xs
        maxY = maximum ys
        (xs, ys) = unzip tups
        tups = concat (map (\(V2 a b) -> [v2p a, v2p b]) lines)
