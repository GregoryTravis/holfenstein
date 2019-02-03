module Diag
( Diag(..)
, DiagT(..)
, Dline(..)
, Ddiamond(..)
, Drawable
, Dpoint(..)
, box
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

class Drawable a where
  toLines :: a -> [V2 (V2 Double)]
  transform :: Trans -> a -> a

data Dline = Dline (V2 (V2 Double)) deriving Show

instance Drawable Dline where
  toLines (Dline line) = [line]
  transform t (Dline line) = Dline $ transformLine t line

data Dpoint = Dpoint (V2 Double) deriving Show

instance Drawable Dpoint where
  toLines (Dpoint v) = box v
  transform t (Dpoint v) = Dpoint $ transformV t v

data Ddiamond = Ddiamond (V2 Double) deriving Show

instance Drawable Ddiamond where
  toLines (Ddiamond v) = diamond v
  transform t (Ddiamond v) = Ddiamond $ transformV t v

data DiagT a b = DiagT (a, b) deriving Show

instance (Drawable x, Drawable y) => Drawable (DiagT x y)  where
  toLines (DiagT (a, b)) = (toLines a) ++ (toLines b)
  transform t (DiagT (a, b)) = DiagT (transform t a, transform t b)

data Diag a = Diag [a] deriving Show

instance Drawable a => Drawable (Diag a) where
  toLines (Diag diagrammables) = concat $ map toLines diagrammables
  transform t (Diag diagrammables) = Diag $ map (transform t) diagrammables

drawDiag :: Drawable a => Window -> a -> IO ()
drawDiag window diag = do
  --msp diag
  --msp $ bbox diag
  --msp $ boxToBoxTransform (V2 (V2 2.0 2.0) (V2 3.0 3.0)) (V2 (V2 1.0 1.0) (V2 3.0 3.0))
  withFramebuffer window foo
  return ()
  where foo :: Ptr Word32 -> Int -> IO ()
        foo ptr pitch = mapM_ (\line -> drawLine window line white ptr pitch) (map toLineLine $ toLines (transform winT diag))
        winT = boxToBoxTransform (bbox diag) (V2 (V2 0.0 0.0) winV)
        winV = case (getDimensions window) of (w, h) -> V2 (fromIntegral (w - 1)) (fromIntegral (h - 1))
        --toLines (Diag lines) = map toLine lines
        toLineLine (V2 a b) = Line (floorV a) (floorV b)

--pointsOf :: Diag -> [(V2 Double)]
--pointsOf (Diag lines) = concat lines

v2p (V2 x y) = (x, y)
p2v (x, y) = V2 x y

-- scale, translate
data Trans = Trans Double (V2 Double) deriving Show

composeTrans (Trans s0 t0) (Trans s1 t1) = Trans (s0 * s1) ((s1 *^ t0) + t1)

--transformDiag :: Drawable a => Trans -> a -> a
--transformDiag t d = Diag (map (transformLine t) lines)
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

bbox :: Drawable a => a -> V2 (V2 Double)
bbox diag = V2 (V2 minX minY) (V2 maxX maxY)
  where minX = minimum xs
        minY = minimum ys
        maxX = maximum xs
        maxY = maximum ys
        (xs, ys) = unzip tups
        tups = concat (map (\(V2 a b) -> [v2p a, v2p b]) (toLines diag))

diamond p = diamondR p 4
diamondR p radius = cycleLines [a, b, c, d]
  where a = p + dx
        b = p + dy
        c = p - dx
        d = p - dy
        dx = V2 (fromIntegral radius) 0.0
        dy = V2 0.0 (fromIntegral radius)

box p = boxR p 4
boxR p radius = cycleLines [a, b, c, d]
  where a = p + dx + dy
        b = p + dx - dy
        c = p - dx - dy
        d = p - dx + dy
        dx = V2 (fromIntegral radius) 0.0
        dy = V2 0.0 (fromIntegral radius)

cycleLines pts = map p2v $ zip pts (cycle1 pts)
  where cycle1 pts = take (length pts) (drop 1 (cycle pts))
