module Util
( assert
, assertM
, esp
, eesp
, eeesp
, feesp
, sp
, msp
, fromLeftReal
, mappily
) where

import Control.Exception.Base
import Data.Either
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import System.IO.Unsafe
import Text.Pretty.Simple (pShow, pShowNoColor)

esp a = unsafePerformIO $ do
  putStrLn $ show $ a
  return a

eesp s a = unsafePerformIO $ do
  putStrLn $ show $ s
  return a

eeesp s a = unsafePerformIO $ do
  putStrLn $ show $ (s, a)
  return a

-- Fake ones for quickly disabling
feesp s a = a

sp x = unpack $ toStrict $ pShowNoColor $ x
msp x = putStrLn $ sp x

-- Really surprised this doesn't exist
fromLeftReal (Left a) = a

assertM :: Show b => b -> Bool -> a -> a
assertM m b a
  | b = a
  | otherwise = unsafePerformIO $ do
      putStrLn $ show m
      return $ assert b a
      --return a

-- I am giving this a terrible name because I know it must exist but I don't
-- know enough to know what it's called and I refused to accept at the moment
-- that it might be called fmap.
mappily :: (a -> b) -> Maybe a -> Maybe b
mappily f (Just x) = Just (f x)
mappily f Nothing = Nothing
