module Util
( assert
, assertM
, esp
, eesp
, feesp
, sp
, msp
, fromLeftReal
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
