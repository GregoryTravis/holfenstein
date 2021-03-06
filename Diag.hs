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

import Grafix
import Img
import Line
import Math
import Util
import Window

data CLine = CLine PackedColor (V2 (V2 Double))

class Drawable a where
  toLines :: a -> [CLine]
  transform :: Trans -> a -> a

data Dline = Dline PackedColor (V2 (V2 Double)) deriving Show

instance Drawable Dline where
  toLines (Dline color line) = [CLine color line]
  transform t (Dline color line) = Dline color $ transformLine t line

data Dpoint = Dpoint PackedColor (V2 Double) deriving Show

instance Drawable Dpoint where
  toLines (Dpoint color v) = map (CLine color) $ box v
  transform t (Dpoint color v) = Dpoint color $ transformV t v

data Ddiamond = Ddiamond PackedColor (V2 Double) deriving Show

instance Drawable Ddiamond where
  toLines (Ddiamond color v) = map (CLine color) $ diamond v
  transform t (Ddiamond color v) = Ddiamond color $ transformV t v

data DiagT a b = DiagT (a, b) deriving Show

instance (Drawable x, Drawable y) => Drawable (DiagT x y)  where
  toLines (DiagT (a, b)) = (toLines a) ++ (toLines b)
  transform t (DiagT (a, b)) = DiagT (transform t a, transform t b)

data Diag a = Diag [a] deriving Show

instance Drawable a => Drawable (Diag a) where
  toLines (Diag diagrammables) = concat $ map toLines diagrammables
  transform t (Diag diagrammables) = Diag $ map (transform t) diagrammables

instance Drawable a => Drawable (Maybe a) where
  toLines (Just x) = toLines x
  toLines Nothing = []
  transform t (Just x) = Just $ transform t x
  transform t Nothing = Nothing

instance Drawable a => Drawable [a] where
  toLines x = concat (map toLines x)
  transform t x = map (transform t) x

drawDiag :: Drawable a => Window -> a -> IO ()
drawDiag window diag = do
  --msp diag
  --msp $ bbox diag
  --msp $ boxToBoxTransform (V2 (V2 2.0 2.0) (V2 3.0 3.0)) (V2 (V2 1.0 1.0) (V2 3.0 3.0))
  withFramebuffer window foo
  return ()
  where foo :: Ptr Word32 -> Int -> IO ()
        foo ptr pitch = do
          floorAndCeiling window ptr pitch
          mapM_ (\(CLine color line) -> drawLine window (screenFlipLine (toLineLine line)) color ptr pitch) $ toLines (transform winT diag)
{-
          drawLine window (Line (V2 (-320) 479) (V2 960 (-0))) white ptr pitch
          drawLine window (Line (V2 960 (-10)) (V2 (-320) 469)) white ptr pitch
          drawLine window (Line (V2 (-320) (-0)) (V2 960 479)) white ptr pitch
          drawLine window (Line (V2 (-320) (-10)) (V2 960 469)) white ptr pitch
          drawLine window (xyFlipLine (Line (V2 (-320) 479) (V2 960 (-0)))) white ptr pitch
          drawLine window (xyFlipLine (Line (V2 960 (-10)) (V2 (-320) 469))) white ptr pitch
          drawLine window (xyFlipLine (Line (V2 (-320) (-0)) (V2 960 479))) white ptr pitch
          drawLine window (xyFlipLine (Line (V2 (-320) (-10)) (V2 960 469))) white ptr pitch
        xyFlipLine (Line (V2 x0 y0) (V2 x1 y1)) = Line (V2 y0 x0) (V2 y1 x1)
-}
        --bb = bbox diag
        bb = V2 (V2 (- n) (- n)) (V2 n n)
        n = 4.0
        winT = boxToBoxTransform bb (V2 (V2 0.0 0.0) winV)
        (winW, winH) = getDimensions window
        winV = V2 (fromIntegral (winW - 1)) (fromIntegral (winH - 1))
        --toLines (Diag lines) = map toLine lines
        toLineLine (V2 a b) = Line (floorV a) (floorV b)
        screenFlipLine (Line a b) = Line (screenFlip a) (screenFlip b)
        screenFlip (V2 x y) = V2 x (winH - y - 1)

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
        tups = concat (map (\(V2 a b) -> [v2p a, v2p b]) (map (\ (CLine _ line) -> line) (toLines diag)))

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
