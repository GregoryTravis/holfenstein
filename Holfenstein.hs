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
import qualified Data.Set as S
import Data.Word (Word32)
import qualified Debug.Trace as TR
import Foreign.C.Types
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr
import Foreign.Storable (peekElemOff, pokeElemOff)
import Linear
import Numeric (showHex)
--import qualified SDL.Vect
import SDL.Event --(mouseMotionEventPos, EventPayload(MouseMotionEvent), MouseMotionEventData)
import SDL.Vect
import SDL.Video.Renderer (lockTexture, unlockTexture)
import SDL (($=), unwrapKeycode, keysymKeycode, unwrapKeycode)
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

drawLine :: Line Int -> Color -> Ptr Word32 -> Int -> IO ()
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

drawLines :: [Line Int] -> Ptr Word32 -> Int -> IO ()
drawLines lines ptr pitch =
  let dl line = drawLine line white ptr pitch
   in mapM_ dl lines

boxAround :: V2 Double -> [Line Double]
boxAround center = box (center - boxOffset) (center + boxOffset)
  where boxOffset = V2 0.05 0.05

toOffset (V2 x y) pitch = y * (pitch `div` 4) + x

inScreenBounds (V2 x y) = x >= 0 && y >= 0 && x < screenWidth && y < screenHeight

drawPoint :: V2 Int -> Color -> Ptr Word32 -> Int -> IO ()
drawPoint v c ptr pitch = do
  assertM (v, ptr, pitch) (inScreenBounds v)
    pokeElemOff ptr (toOffset v pitch) (packColor c)

toRad degrees = 2 * pi * ((fromIntegral degrees) / 360.0)

whup :: V2 Double -> V2 Int
whup (V2 x y) = V2 (floor x) (floor y)

circlePointsF :: Double -> Double -> Double -> [V2 Double]
--circlePoints r a s | TR.trace (show ("cp", r, a, s)) False = undefined
circlePointsF radius startAng step = map cp [startAng, startAng + step .. pi * 2]
  where cp ang = V2 ((cos ang) * radius) ((sin ang) * radius)
circlePoints r sA s = map whup $ circlePointsF r sA s

clearCanvas2 :: Ptr Word32 -> Int -> IO ()
clearCanvas2 wordPtr pitch = fillBytes wordPtr (fromIntegral 0) (pitch * screenHeight)

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

data Line a = Line (V2 a) (V2 a) deriving Show
scale :: Num a => V2 a -> Line a -> Line a
scale s (Line a b) = Line (s * a) (s * b)
translate v (Line a b) = Line (v + a) (v + b)
scaleLines s lines = map (scale s) lines
translateLines v lines = map (translate v) lines

box :: V2 a -> V2 a -> [Line a]
box (V2 x0 y0) (V2 x1 y1) = [Line a b, Line b c, Line c d, Line d a]
  where a = V2 x0 y0
        b = V2 x1 y0
        c = V2 x1 y1
        d = V2 x0 y1

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

transposeAA ([]:_) = []
transposeAA xs = (map head xs) : transposeAA (map tail xs)

type World = [[Bool]]

world :: World
world = map (\col -> map isWall col) (transposeAA worldMap)
  where 
    isWall x = x == '#'
worldTransposed = transposeAA world

allSameLength xs = length (nub (map length xs)) == 1
worldIsSquare = allSameLength world
blah = assert worldIsSquare ()
worldSize world = V2 (length world) (length (world !! 0))
outsideWorld :: World -> Int -> Int -> Bool
outsideWorld world x y = x < 0 || y < 0 || x >= w || y >= h
  where V2 w h = worldSize world
outsideWorldF :: World -> V2 Double -> Bool
outsideWorldF world (V2 x y) = case worldSize world of (V2 wx wy) -> x < 0 || x >= (fromIntegral wx) + 1 || y < -1 || y > (fromIntegral wy) + 1

--isHorWall w x y | TR.trace (show ("isHorWall", x, y)) False = undefined
isHorWall world x y = (isSolid world x (y - 1)) /= (isSolid world x y)
isVerWall world x y = (isSolid world (x - 1) y) /= (isSolid world x y)
isSolid :: World -> Int -> Int -> Bool
--isSolid x y | TR.trace (show ("iS", x, y, (length world), worldSize, (outsideWorld x y))) False = undefined
isSolid world x y
  | outsideWorld world x y = False
  | otherwise = ((world !! x) !! y)

horToLine x y = Line (V2 x y) (V2 (x + 1) y)
verToLine x y = Line (V2 x y) (V2 x (y + 1))

allWalls world = [horToLine x y | x <- [0..w], y <- [0..h], isHorWall world x y] ++ [verToLine x y | x <- [0..w], y <- [0..h], isVerWall world x y]
  where V2 w h = worldSize world

data WallPt = Ver Int Double | Hor Double Int deriving Show
wallPtToV2 (Ver x y) = V2 (fromIntegral x) y
wallPtToV2 (Hor x y) = V2 x (fromIntegral y)
transposeHit (Hor x y) = Ver y x
transposeHit (Ver x y) = Hor y x
transposeMaybeHit (Just hit) = Just $ transposeHit hit
transposeMaybeHit Nothing = Nothing

transposeV2 (V2 x y) = V2 y x

doCastTr = False
castTr s b | doCastTr = TR.trace s b
           | otherwise = b

castRay :: World -> V2 Double -> V2 Double -> Maybe WallPt
castRay w a b | castTr ("castRay " ++ (show a) ++ " " ++ (show b)) False = undefined
castRay world eye@(V2 ex ey) dir@(V2 dx dy)
  | (abs dy) <= (abs dx) = stepRay world eye (eye + firstStep) unitStep slope
  -- This is a terribly egregious hack
  | otherwise = transposeMaybeHit (castRay worldTransposed (transposeV2 eye) (transposeV2 dir))
  where slope = dy / dx
        firstStep = V2 firstVerDx firstVerDy
        firstVerDx | dx > 0 = (fromIntegral (ceiling ex)) - ex
                   | dx < 0 = (fromIntegral (floor ex)) - ex
        firstVerDy = firstVerDx * slope
        unitStep = V2 (signum dx) ((signum dx) * slope)

-- (x1, y1) is always on a vertical grid line; (x0, y0) is the previous one or
-- the initial eye point.
stepRay :: World -> V2 Double -> V2 Double -> V2 Double -> Double -> Maybe WallPt
stepRay w p0 p1 u s | castTr (show ("stepRay", p0, p1, u, 2)) False = undefined
stepRay world p0@(V2 x0 y0) p1@(V2 x1 y1) unitStep slope
  | (floor y0) /= (floor y1) && isHorWall world (floor (min x0 x1)) (floor (max y0 y1)) && (abs slope) > 0 = Just $ Hor (x0 + (((fromIntegral (floor (max y0 y1))) - y0) / slope)) (floor (max y0 y1))
  | isVerWall world (floor x1) (floor y1) = Just $ Ver (floor x1) y1
  | outsideWorldF world p0 = Nothing
  | otherwise = stepRay world p1 (p1 + unitStep) unitStep slope

toWorldCoordinate :: Int -> Double
toWorldCoordinate ix = ((fromIntegral ix)-100.0) / 50.0

forDisplay :: Num a => [Line a] -> [Line a]
forDisplay lines = 
  translateLines (V2 100 100) (scaleLines 50 lines)
forDisplayF :: [Line Double] -> [Line Int]
forDisplayF lines = map floorL (forDisplay lines)
  where floorL (Line a b) = Line (floorV a) (floorV b)
        floorV (V2 x y) = V2 (floor x) (floor y)

drawMap t = withFramebuffer t (drawLines map)
  where map = forDisplay $ allWalls world -- translateLines (V2 100 100) (scaleLines 50 allWalls)

--thing t = withFramebuffer t $ castAndShow (V2 1.5 1.5) (V2 1.0 0.5)
thing eye ang t = withFramebuffer t $ castAndShowL eye dirs
  where castAndShow eye dir ptr pitch = do
          let hit = castRay world eye (signorm dir)
          drawLines (forDisplayF (boxAround eye)) ptr pitch
          mapM_ (\pt -> drawLines (forDisplayF (boxAround pt)) ptr pitch) vpps
        --drawLines (forDisplayF [(Line eye (eye + dir))]) ptr pitch
          case hit of
            Just hit -> do
              drawLines (forDisplayF (boxAround (wallPtToV2 hit))) ptr pitch
              drawLines (forDisplayF [(Line eye (wallPtToV2 hit))]) ptr pitch
            Nothing -> return ()
          --putStrLn $ show (hit, dir)
          return hit
        castAndShowL eye dirs ptr pitch = mapM_ (\dir -> castAndShow eye dir ptr pitch) dirs
        --dirs = [V2 1.0 0.5, V2 1.0 (-0.5)]
        --dirs = circlePointsF 1.0 0 (pi / 32)
        vpps = viewPlanePoints eye ang
        dirs = map (\vpp -> signorm (vpp - eye)) vpps
        --dirs = [V2 9.801714032956077e-2 0.9951847266721968, V2 (-9.801714032956077e-2) 0.9951847266721968]
        --dirs = [V2 (-0.9) (-1.0)]
--circlePoints radius startAng step = map cp [startAng, startAng + step .. pi * 2]

vroo = do
  putStrLn $ show $ V2 3.4 4.5
  putStrLn $ show $ (V2 3.4 4.5) * 2
  putStrLn $ show $ Ver 1 2.3
  putStrLn $ show world
  putStrLn $ show $ (signorm (V2 1.0 0.5))
  return ()

--data PressRelease = Press | Release
data KeyEvent = KeyEvent Int InputMotion deriving (Eq, Ord, Show)
type KeySet = S.Set Int

getKeyEvents :: [EventPayload] -> [KeyEvent]
getKeyEvents events = map toKeyEvent $ filter isKeyboardEvent events
  where keyboardEvents = filter isKeyboardEvent events
        isKeyboardEvent (KeyboardEvent _) = True
        isKeyboardEvent _ = False
        getPressRelease :: EventPayload -> InputMotion
        getPressRelease (KeyboardEvent ke) = keyboardEventKeyMotion ke
        getCode (KeyboardEvent ke) = fromIntegral $ unwrapKeycode (keysymKeycode (keyboardEventKeysym ke))
        toKeyEvent event = KeyEvent (getCode event) (getPressRelease event)

updateKeySet :: KeySet -> [KeyEvent] -> KeySet
updateKeySet keySet (KeyEvent scanCode Pressed : kes) = updateKeySet (S.insert scanCode keySet) kes
updateKeySet keySet (KeyEvent scanCode Released : kes) = updateKeySet (S.delete scanCode keySet) kes
updateKeySet keySet [] = keySet

getCursorPos :: [EventPayload] -> Maybe (Int, Int)
getCursorPos events = case (filter isMouseMotionEvent events) of [] -> Nothing
                                                                 es -> case (last es) of (MouseMotionEvent d) -> case (mouseMotionEventPos d) of (P (V2 x y)) -> Just (fromIntegral x, fromIntegral y)
  where isMouseMotionEvent (MouseMotionEvent _) = True
        isMouseMotionEvent _ = False
screenToWorld (x, y) = (toWorldCoordinate x, toWorldCoordinate y)

fov = pi / 3
-- view plane starts one unit from origin perp to the x axis
viewPlaneWidth = 2.0 * tan (fov / 2)
viewPlaneLeft = V2 1.0 (viewPlaneWidth / 2)
viewPlaneRight = V2 1.0 (-(viewPlaneWidth / 2))

rotMatrix ang = V2 (V2 c (-s)) (V2 s c)
  where c = cos ang
        s = sin ang

scaleV (V2 x y) s = V2 (s * x) (s * y)

lerpVV :: (V2 Double, V2 Double) -> V2 Double -> V2 Double
lerpVV (left, right) k = (k * right) + ((1.0 - k) * left)
lerpVVs :: (V2 Double, V2 Double) -> Int -> [V2 Double]
lerpVVs lr count = map (lerpVV lr) $ map (step *) $ map fromIntegral [0..(count - 1)]
  where step = 1.0 / (fromIntegral (count - 1))

--foo :: V2 double
--foo = (V2 (V2 1.0 1.0) (V2 1.0 1.0)) !*! (V2 1.0 1.0)

multMV :: V2 (V2 Double) -> V2 Double -> V2 Double
multMV (V2 (V2 x0 y0) (V2 x1 y1)) (V2 x y) = V2 ((x0 * x) + (y0 * y)) ((x1 * x) + (y1 * y))

viewPlanePoints :: V2 Double -> Double -> [V2 Double]
viewPlanePoints eye ang = lerpVVs rotatedViewPlane 7 -- screenWidth)
  where rotatedViewPlane = ((multMV rM viewPlaneLeft) + eye, (multMV rM viewPlaneRight) + eye)
        rM = rotMatrix ang

castDirs :: V2 Double -> Double -> [V2 Double]
castDirs eye ang = map dirForColumn $ viewPlanePoints eye ang
  where dirForColumn :: V2 Double -> V2 Double
        dirForColumn pos = signorm (eye - pos)

boxPoints t pts = withFramebuffer t foo
  where foo ptr pitch = do
          mapM_ bar pts
            where bar pt = drawLines (forDisplayF (boxAround pt)) ptr pitch

bong eye t = boxPoints t $ viewPlanePoints eye (pi / 4)

--thang :: V2 Double -> V2 Double -> IO ()
thang eye dir t = withFramebuffer t $ thung eye dir
  where thung eye dir ptr pitch = do putStrLn $ show ("thung", eye, dir)
                                     return ()

main :: IO ()
main = do
  --putStrLn $ show (map length world, nub (map length world))
  --putStrLn $ show blah
  --vroo
  --putStrLn $ show $ multMV (V2 (V2 1.0 2.0) (V2 3.0 4.0)) (V2 10.0 100.0)
  --let rm = rotMatrix (pi / 4)
  --msp rm
  --msp $ multMV rm (V2 1.0 0.0)
  --msp $ multMV (rotMatrix (pi / 8)) (V2 1.0 0.0)
  --msp $ multMV (rotMatrix (-(pi / 8))) (V2 1.0 0.0)
  --msp $ multMV (rotMatrix ((3 * pi) / 4)) (V2 1.0 0.0)
  --msp $ multMV (rotMatrix ((3 * pi) / 4)) (V2 0.0 1.0)
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
  --vroo

  let
    screenCenter = P (V2 (fromIntegral (screenWidth `div` 2)) (fromIntegral (screenHeight `div` 2)))

    loop theta prevEye prevAng keySet = do
      --putStrLn $ "LOOP " ++ (show theta)
      --withFramebuffer targetTexture $ goof3 theta
      --withFramebuffer targetTexture goof2
      withFramebuffer targetTexture clearCanvas2
      drawMap targetTexture
      --let eye = (V2 1.1 1.1) + ((V2 20.0 15.0) * ((fromIntegral theta) / 360.0))
      --let eye = (V2 6.6 1.1) + ((V2 0.0 15.0) * ((fromIntegral theta) / 360.0))
      --let eye = (V2 3.9 3.2)
      events <- map SDL.eventPayload <$> SDL.pollEvents
      --msp events
      let keyEvents = getKeyEvents events
      let newKeySet = updateKeySet keySet keyEvents
      if newKeySet /= keySet then msp newKeySet else return ()
      let quit = SDL.QuitEvent `elem` events || S.member 27 newKeySet
      let eye = case getCursorPos events of Just (x, y) -> case screenToWorld (x, y) of (x, y) -> (if outsideWorldF world (V2 x y) then prevEye else (V2 x y))
                                            Nothing -> prevEye

      --putStrLn $ show $ eye
      let ang = prevAng
      --msp $ ("ang", ang)
      thing eye ang targetTexture
      --thang eye ang targetTexture
      --bong eye targetTexture

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

      unless quit (loop (theta + 2 `mod` 360) eye ang newKeySet)

  loop (0 :: Int) (V2 1.6 5.3) (pi / 4) S.empty

  SDL.destroyWindow window
  SDL.quit
