module Tex
( Tex(..)
, imgToTex
) where

import Data.Word (Word32)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import Foreign.Storable (pokeElemOff)

import Img

data Tex = Tex Int Int (Ptr Word32)
instance Show Tex where
  show (Tex w h _) = show (w, h)

imgToTex :: Img -> IO Tex
imgToTex im@(Img colors w h) = do
  mem <- mallocBytes (w * h * 4) :: IO (Ptr Word32)
  mapM_ (copy mem w) [(x, y) | x <- [0..w-1], y <- [0..h-1]]
  return $ Tex w h mem
  where copy mem w (x, y) = do pokeElemOff mem (x + (y * w)) (packColor (pixAt im x y))
