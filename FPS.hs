module FPS
( fps
, frBufferEmpty
) where

import Data.Time.Clock.POSIX (getPOSIXTime)

frBufferLen = 20
data FRBuffer = FRBuffer [Double] Double Int
frBufferEmpty = FRBuffer [] 0 0
frBufferAvg (FRBuffer _ _ 0) = 0
frBufferAvg (FRBuffer es tot num) = tot / (fromIntegral num)
frBufferAdd (FRBuffer es tot num) e | num == frBufferLen = FRBuffer [e] e 1
                                    | otherwise = FRBuffer (e : es) (tot + e) (num + 1)
frBufferUpdate :: FRBuffer -> Double -> (Double, FRBuffer)
frBufferUpdate frBuf e = (frBufferAvg frBuf, frBufferAdd frBuf e)

fps lastNow frBuf = do
  now <- getPOSIXTime 
  let instantFPS :: Double
      instantFPS = 1.0 / (realToFrac (now - lastNow))
  let (fps, newFRBuf) = frBufferUpdate frBuf instantFPS
  return (now, fps, newFRBuf)
