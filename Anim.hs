module Anim where

import Diag

data Anim a = Anim (Double -> a)

frameAt :: Drawable a => Anim a -> Double -> a
frameAt (Anim f) t = f t
