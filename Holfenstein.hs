{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (any, mapM_)
import Control.Exception.Base
import Control.Monad hiding (mapM_)
import Data.Bits
import Data.Foldable hiding (elem)
import Data.List (nub)
import Data.Maybe
import Data.Ord
import Data.Word (Word32)
import qualified Debug.Trace as TR
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable (peekElemOff, pokeElemOff)
import Linear
import Numeric (showHex)
import SDL.Vect
import SDL.Video.Renderer (lockTexture, unlockTexture)
import SDL (($=))
import qualified SDL
import System.Exit
import System.IO

import Util

-- #if !MIN_VERSION_base(4,8,0)
-- import Control.Applicative
-- #endif

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = (640, 480)

data Texture = Texture SDL.Texture (V2 CInt)

createBlank :: SDL.Renderer -> V2 CInt -> SDL.TextureAccess -> IO Texture
createBlank r sz access = Texture <$> SDL.createTexture r SDL.RGBA8888 access sz <*> pure sz

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> Maybe CDouble -> Maybe (Point V2 CInt) -> Maybe (V2 Bool) -> IO ()
renderTexture r (Texture t size) xy clip theta center flips =
  let dstSize =
        maybe size (\(SDL.Rectangle _ size') -> size') clip
  in SDL.copyEx r
                t
                clip
                (Just (SDL.Rectangle xy dstSize))
                (fromMaybe 0 theta)
                center
                (fromMaybe (pure False) flips)

setAsRenderTarget :: SDL.Renderer -> Maybe Texture -> IO ()
setAsRenderTarget r Nothing = SDL.rendererRenderTarget r $= Nothing
setAsRenderTarget r (Just (Texture t _)) = SDL.rendererRenderTarget r $= Just t

data Color = Color Int Int Int
white = Color 255 255 255

packColor :: Color -> Word32
packColor (Color r g b) =
  fromIntegral $ (shift r 24) .|. (shift g 16) .|. (shift b 8) .|. 0xff

colorFade x y =
  (shift red 24) .|. (shift green 16) .|. 0xff
  where red = floor $ 255 * ((fromIntegral x) / (fromIntegral screenWidth))
        green = floor $ 255 * ((fromIntegral y) / (fromIntegral screenHeight))

withFramebuffer :: Texture -> (Ptr Word32 -> Int -> IO a) -> IO a
withFramebuffer (Texture t _) f = do
  (ptr, (CInt pitch)) <- lockTexture t Nothing
  let wordPtr :: Ptr Word32
      wordPtr = castPtr ptr
  result <- f wordPtr (fromIntegral pitch :: Int)
  unlockTexture t
  return result

drawLine :: Line -> Color -> Ptr Word32 -> Int -> IO ()
drawLine (Line a@(V2 x0 y0) (V2 x1 y1)) color ptr pitch = step fa delta count
  where delta | isVert = V2 ((signum dx) * (abs (dx / dy))) (signum dy)
              | otherwise = V2 (signum dx) ((signum dy) * (abs (dy / dx)))
        count | isVert = abs idy
              | otherwise = abs idx
        fa :: V2 Double
        fa = V2 (fromIntegral x0) (fromIntegral y0)
        isVert = (abs dy) > (abs dx)
        idx = x1 - x0
        idy = y1 - y0
        dx :: Double
        dx = fromIntegral idx
        dy :: Double
        dy = fromIntegral idy
        step :: V2 Double -> V2 Double -> Int -> IO ()
        --step a d c | TR.trace ("step " ++ (show a) ++ " " ++ (show d) ++ " " ++ (show c)) False = undefined
        step a@(V2 x y) delta count
          | count == 0 = return ()
          | otherwise = do
              drawPoint (V2 (floor x) (floor y)) color ptr pitch
              step (a + delta) delta (count - 1)
  --where (delta, count) = bleh a b
        --bleh :: V2 Int -> V2 Int -> (V2 Double, Int)
        --bleh a b | isVert a b -> ((V2 (dx / dy) 1)

drawLines :: [Line] -> Ptr Word32 -> Int -> IO ()
drawLines lines ptr pitch =
  let dl line = drawLine line white ptr pitch
   in mapM_ dl lines

toOffset (V2 x y) pitch = y * (pitch `div` 4) + x

inScreenBounds (V2 x y) = x >= 0 && y >= 0 && x < screenWidth && y < screenHeight

drawPoint :: V2 Int -> Color -> Ptr Word32 -> Int -> IO ()
drawPoint v c ptr pitch = do
  assertM (v, ptr, pitch) (inScreenBounds v)
    pokeElemOff ptr (toOffset v pitch) (packColor c)

toRad degrees = 2 * pi * ((fromIntegral degrees) / 360.0)

whup :: V2 Double -> V2 Int
whup (V2 x y) = V2 (floor x) (floor y)

circlePoints :: Double -> Double -> Double -> [V2 Int]
--circlePoints r a s | TR.trace (show ("cp", r, a, s)) False = undefined
circlePoints radius startAng step = map cp [startAng, startAng + step .. pi * 2]
  where cp ang = whup $ V2 ((cos ang) * radius) ((sin ang) * radius)

goof2 :: Ptr Word32 -> Int -> IO ()
goof2 wordPtr pitch = do
  let writeFade (x, y) = let off = y * (pitch `div` 4) + x
                          in pokeElemOff wordPtr off (colorFade x y)
  mapM_ writeFade [(x, y)
                      | x <- [0..((fromIntegral screenWidth :: Int)-1)]
                      , y <- [0..((fromIntegral screenHeight :: Int)-1)]]
  drawPoint (V2 (screenWidth `div` 2) (screenHeight `div` 2)) (Color 255 255 255) wordPtr pitch
  drawLine (Line (V2 100 100) (V2 200 150)) (Color 255 0 0) wordPtr pitch
  drawLine (Line (V2 100 100) (V2 150 200)) (Color 255 0 0) wordPtr pitch
  drawLine (Line (V2 209 159) (V2 109 109)) (Color 0 255 0) wordPtr pitch
  drawLine (Line (V2 159 209) (V2 109 109)) (Color 0 255 0) wordPtr pitch
  --let dl (x, y) = drawLine (Line (V2 x y) (V2 320 240)) (Color 255 255 255) wordPtr pitch
   --in mapM_ dl ([(x, 200) | x <- [0, 10 .. 639]] ++
                --[(x, 280) | x <- [0, 10 .. 639]])
  return ()

goof3 theta wordPtr pitch = do
  let dl' (V2 x y) = drawLine (Line (V2 x y) (V2 320 240)) (Color 128 255 255) wordPtr pitch
      step = 10
      startAng = toRad $ fromIntegral $ (floor ((fromIntegral theta) / 10.0)) `mod` step
   in mapM_ dl' [p + (V2 320 240) | p <- circlePoints 100 startAng (toRad step)]
  return ()

data Line = Line (V2 Int) (V2 Int) deriving Show
scale s (Line a b) = Line (s * a) (s * b)
translate v (Line a b) = Line (v + a) (v + b)
scaleLines s lines = map (scale s) lines
translateLines v lines = map (translate v) lines

-- World is made of unit cubes
-- Cubes are identified by their least corner coords
-- Walls are identified by their least coord along their axis
-- Remember these are drawn upside down
worldMap :: [[Char]]
worldMap = [
  "########",
  "#    # #",
  "#    # #",
  "# #    #",
  "# #    #",
  "########" ]
worldMap__ = [
  "###",
  "  #",
  "###"]
worldMap_ = [
  "####",
  "#  #",
  "#  #",
  "## #"]

world :: [[Bool]]
world = map (\col -> map isWall col) (transpose worldMap)
  where 
    isWall x = x == '#'
    transpose ([]:_) = []
    transpose xs = (map head xs) : transpose (map tail xs)

allSameLength xs = length (nub xs) == 1
worldIsSquare = allSameLength world
blah = assert worldIsSquare ()
worldSize = V2 (length world) (length (world !! 0))
outsideWorld :: Int -> Int -> Bool
outsideWorld x y = x < 0 || y < 0 || x >= w || y >= h
  where V2 w h = worldSize
outsideWorldF :: V2 Double -> Bool
outsideWorldF (V2 x y) = case worldSize of (V2 wx wy) -> x < 0 || x >= (fromIntegral wx) + 1 || y < -1 || y > (fromIntegral wy) + 1

isHorWall x y = (isSolid x (y - 1)) /= (isSolid x y)
isVerWall x y = (isSolid (x - 1) y) /= (isSolid x y)
isSolid :: Int -> Int -> Bool
--isSolid x y | TR.trace (show ("iS", x, y, (length world), worldSize, (outsideWorld x y))) False = undefined
isSolid x y
  | outsideWorld x y = False
  | otherwise = ((world !! x) !! y)

horToLine x y = Line (V2 x y) (V2 (x + 1) y)
verToLine x y = Line (V2 x y) (V2 x (y + 1))

allWalls = [horToLine x y | x <- [0..w], y <- [0..h], isHorWall x y] ++ [verToLine x y | x <- [0..w], y <- [0..h], isVerWall x y]
  where V2 w h = worldSize

data WallPt = Ver Int Double | Hor Double Int deriving Show
wallPtToV2 (Ver x y) = V2 (fromIntegral x) y
wallPtToV2 (Hor x y) = V2 x (fromIntegral y)

castRay :: V2 Double -> V2 Double -> Maybe WallPt
castRay a b | TR.trace ("castRay " ++ (show a) ++ " " ++ (show b)) False = undefined
castRay eye@(V2 ex ey) dir@(V2 dx dy) =
  let dummy = assert ((abs dy) < (abs dx)) ()
   in stepRay eye (eye + firstStep) unitStep slope
  where slope = dy / dx
        firstStep = V2 firstVerDx firstVerDy
        firstVerDx | dx > 0 = (fromIntegral (ceiling ex)) - ex
                   | dx < 0 = (fromIntegral (floor ex)) - ex
        firstVerDy = firstVerDx * slope
        unitStep = V2 1.0 slope

-- (x1, y1) is always on a vertical grid line; (x0, y0) is the previous one or
-- the initial eye point.
stepRay :: V2 Double -> V2 Double -> V2 Double -> Double -> Maybe WallPt
stepRay p0 p1 u s | TR.trace (show "stepRay " ++ (show p0) ++ " " ++ (show p1) ++ " " ++ (show u) ++ " " ++ (show s)) False = undefined
stepRay p0@(V2 x0 y0) p1@(V2 x1 y1) unitStep slope
  | (floor y0) /= (floor y1) && isHorWall (floor x0) (floor y1) && (abs slope) > 0 = Just $ Hor (x0 + (((fromIntegral (floor y1)) - y0) / slope)) (floor y1)
  | isVerWall (floor x1) (floor y1) = Just $ Ver (floor x1) y1
  | outsideWorldF p0 = Nothing
  | otherwise = stepRay p1 (p1 + unitStep) unitStep slope

{-
castRay :: V2 Double -> V2 Double -> Maybe WallPt
castRay eye dir =
  case (castRayHor eye dir, castRayVer eye dir) of
    (Just horHit, Just verHit) -> Just $ closerOf horHit verHit
    (Just horHit, _) -> horHit
    (_, Just verHit) -> verHit
    _ -> Nothing
  where closerOf horHit verHit =
          head $ sortBy (comparing closeToEye) [horHit, verHit]
        closeToEye wpt = distance (wallPtToV2 wwallPtToV2 pt) eye

-- These assume the map is enclosed
-- Find first intersection with a horizontal wall
caseRayHor (V2 eyeX eyeY) (V2 dirX 0) = Nothing
caseRayHor (V2 eyeX eyeY) (V2 dirX dirY) =
  where firstIntersection = Hor firstX first Y
        firstX =
        firstY | dirY > 0 = ceiling eyeY
               | dirY < 0 = floor eyeY

--castRay :: V2
-}

drawMap t = withFramebuffer t (drawLines map)
  where map = translateLines (V2 100 100) (scaleLines 50 allWalls)

vroo = do
  putStrLn $ show $ V2 3.4 4.5
  putStrLn $ show $ (V2 3.4 4.5) * 2
  putStrLn $ show $ Ver 1 2.3
  putStrLn $ show world
  putStrLn $ show $ (signorm (V2 1.0 0.5))
  putStrLn $ show $ castRay (V2 1.5 1.5) (signorm (V2 1.0 0.5))
  return ()

main :: IO ()
main = do
  vroo
  --exitWith ExitSuccess

  hSetBuffering stdout NoBuffering
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowInitialSize = V2 (fromIntegral screenWidth) (fromIntegral screenHeight)}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = False
        }

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  targetTexture <- createBlank renderer (V2 (fromIntegral screenWidth) (fromIntegral screenHeight)) SDL.TextureAccessStreaming
  withFramebuffer targetTexture goof2
  vroo

  let
    screenCenter = P (V2 (fromIntegral (screenWidth `div` 2)) (fromIntegral (screenHeight `div` 2)))

    loop theta = do
      --putStrLn $ "LOOP " ++ (show theta)
      --withFramebuffer targetTexture $ goof3 theta
      drawMap targetTexture
      events <- map SDL.eventPayload <$> SDL.pollEvents
      let quit = SDL.QuitEvent `elem` events

{-
      setAsRenderTarget renderer (Just targetTexture)

      SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
      SDL.clear renderer

      SDL.rendererDrawColor renderer $= V4 maxBound 0 0 maxBound
      SDL.fillRect renderer (Just $ SDL.Rectangle (P $ V2 (screenWidth `div` 4) (screenHeight `div` 4))
                                                        (V2 (screenWidth `div` 2) (screenHeight `div` 2)))

      SDL.rendererDrawColor renderer $= V4 0 0 maxBound maxBound
      SDL.drawRect renderer (Just (SDL.Rectangle (P $ V2 (screenWidth `div` 6) (screenHeight `div` 6))
                                                       (V2 (screenWidth * 2 `div` 3) (screenHeight * 2 `div` 3))))

      SDL.rendererDrawColor renderer $= V4 0 maxBound 0 maxBound
      SDL.drawLine renderer (P (V2 0 (screenHeight `div` 2))) (P (V2 screenWidth (screenHeight `div` 2)))

      SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
      for_ [0, 4 .. screenHeight] $ \i ->
        SDL.drawPoint renderer (P (V2 (screenWidth `div` 2) i))
-}

      setAsRenderTarget renderer Nothing

      --renderTexture renderer targetTexture 0 Nothing (Just (fromIntegral theta)) (Just screenCenter) Nothing
      renderTexture renderer targetTexture 0 Nothing (Just 0) (Just screenCenter) Nothing

      SDL.present renderer

      unless quit (loop (theta + 2 `mod` 360))

  loop (0 :: Int)

  SDL.destroyWindow window
  SDL.quit
