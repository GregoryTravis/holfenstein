{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (any, mapM_)
import Control.Exception.Base
import Control.Monad hiding (mapM_)
import Control.Monad.ST
import Data.Array.ST
import Data.Bits
import Data.Char (ord, toUpper)
import Data.Foldable hiding (elem)
import Data.List (nub)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Data.Word (Word32)
import qualified Debug.Trace as TR
import System.Directory
import Foreign.C.Types
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr
import Foreign.Storable (peek, poke, peekElemOff, pokeElemOff)
import Linear
import Numeric (showHex)
import System.CPUTime (getCPUTime)
import System.Exit
import System.IO

import Gfx
import FPS
import Img
import Line
import Map
import Math
import Physics
import Tex
import Util
import Window

-- #if !MIN_VERSION_base(4,8,0)
-- import Control.Applicative
-- #endif

showMap = True
ifShowMap :: IO () -> IO ()
ifShowMap io = if showMap then io else return ()

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = (640, 480)
--(screenWidth, screenHeight) = (320, 240)

calcTexCoord :: V2 Int -> V2 Double -> Int -> Double
calcTexCoord (V2 sy0 sy1) (V2 ty0 ty1) sy =
  ty0 + (((syf - sy0f) / (sy1f - sy0f)) * (ty1 - ty0))
    where sy0f = fromIntegral sy0
          sy1f = fromIntegral sy1
          syf = fromIntegral sy

-- Native
fastestTextureVStripH :: Double -> VStrip -> Ptr Word32 -> Int -> IO ()
fastestTextureVStripH horPos v@(VStrip x y0 y1 (Tex w h texPtr)) ptr pitch =
  fastestTextureVStrip startPtr texPtr (fromIntegral w) (fromIntegral h) (fromIntegral dPtr) tx (fromIntegral cy0) (fromIntegral cy1) fty0 dfty
  where startPtr = plusPtr ptr ((cy0 * pitch) + (x*4))
        dPtr = pitch `div` 4
        tx = floor (horPos * (fromIntegral w))
        fty :: Int -> CDouble
        fty y = realToFrac $ calcTexCoord (V2 y0 y1) (V2 0.0 (fromIntegral h)) y
        fty0 :: CDouble
        fty0 = fty cy0
        dfty :: CDouble
        dfty = (fty 1) - (fty 0)
        -- clipped screen coords
        (cy0, cy1) = case clipToScreen v of (VStrip _ cy0 cy1 tex) -> (cy0, cy1)

drawLine :: Line Int -> PackedColor -> Ptr Word32 -> Int -> IO ()
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
        step a@(V2 x y) delta count
          | count == 0 = return ()
          | otherwise = do
              drawPoint (V2 (floor x) (floor y)) color ptr pitch
              step (a + delta) delta (count - 1)

drawLines :: [Line Int] -> Ptr Word32 -> Int -> IO ()
drawLines lines ptr pitch =
  let dl line = drawLine line white ptr pitch
   in mapM_ dl lines

toOffset (V2 x y) pitch = y * (pitch `div` 4) + x

inScreenBounds (V2 x y) = x >= 0 && y >= 0 && x < screenWidth && y < screenHeight

drawPoint :: V2 Int -> PackedColor -> Ptr Word32 -> Int -> IO ()
drawPoint v c ptr pitch = do
  assertM (v, ptr, pitch) (inScreenBounds v)
    pokeElemOff ptr (toOffset v pitch) c

toRad degrees = 2 * pi * ((fromIntegral degrees) / 360.0)

whup :: V2 Double -> V2 Int
whup (V2 x y) = V2 (floor x) (floor y)

clearCanvas2 :: Ptr Word32 -> Int -> IO ()
clearCanvas2 wordPtr pitch = do
  fillBytes wordPtr (fromIntegral 84) half
  fillBytes (plusPtr wordPtr half) (fromIntegral 40) half
  return ()
  where half = (pitch * screenHeight) `div` 2

transposeAA ([]:_) = []
transposeAA xs = (map head xs) : transposeAA (map tail xs)

-- Just one texture in the world for now
data WorldTexMap = WorldTexMap (M.Map Char Tex)
--getTexForHit w h (WorldTexMap t) | TR.trace (show ("gT", h, t, floorV $ wallPtToV2 h, (case (floorV $ wallPtToV2 h) of (V2 x y) -> getMaterial world x y))) False = undefined
darkenInterpolated = False
getTexForHit frab (Hit hit interpolated) (WorldTexMap t) = fromJust $ M.lookup (darkenMaybe texChar) t
  where texChar = case solidOf (sidesOf hit) of (x, y) -> getMaterial frab x y
        -- Non-exhaustive cases: we assert here that exactly one side of the hit is solid
        solidOf ((x0, y0), (x1, y1))
          | solid0 && (not solid1) = (x0, y0)
          | (not solid0) && solid1 = (x1, y1)
          where solid0 = isSolid frab x0 y0
                solid1 = isSolid frab x1 y1
        sidesOf (Hor fx y) = let x = floor fx in ((x, y-1), (x, y))
        sidesOf (Ver x fy) = let y = floor fy in ((x-1, y), (x, y))
        darkenMaybe c = if darkenInterpolated && interpolated then (toUpper c) else c

allSameLength xs = length (nub (map length xs)) == 1
transposeV2 (V2 x y) = V2 y x

doCastTr = False
castTr s b | doCastTr = TR.trace s b
           | otherwise = b

castRay :: Frab -> Frab -> V2 Double -> V2 Double -> Maybe WallPt
--castRay w a b | castTr ("castRay " ++ (show a) ++ " " ++ (show b)) False = undefined
castRay frab frabT eye@(V2 ex ey) dir@(V2 dx dy)
  | (abs dy) <= (abs dx) = stepRay frab eye (eye + firstStep) unitStep slope
  | otherwise = transposeMaybeHit (castRay frabT frab (transposeV2 eye) (transposeV2 dir))
  where slope = dy / dx
        firstStep = V2 firstVerDx firstVerDy
        firstVerDx | dx > 0 = (fromIntegral (ceiling ex)) - ex
                   | dx < 0 = (fromIntegral (floor ex)) - ex
        firstVerDy = firstVerDx * slope
        unitStep = V2 (signum dx) ((signum dx) * slope)

-- (x1, y1) is always on a vertical grid line; (x0, y0) is the previous one or
-- the initial eye point.
stepRay :: Frab -> V2 Double -> V2 Double -> V2 Double -> Double -> Maybe WallPt
--stepRay w p0 p1 u s | castTr (show ("stepRay", p0, p1, u, 2)) False = undefined
stepRay frab p0@(V2 x0 y0) p1@(V2 x1 y1) unitStep slope
  | (floor y0) /= (floor y1) && isHorWall frab (floor (min x0 x1)) (floor (max y0 y1)) && (abs slope) > 0 = Just $ Hor (x0 + (((fromIntegral (floor (max y0 y1))) - y0) / slope)) (floor (max y0 y1))
  | isVerWall frab (floor x1) (floor y1) = Just $ Ver (floor x1) y1
  | outsideWorldF frab p0 = Nothing
  | otherwise = stepRay frab p1 (p1 + unitStep) unitStep slope

data Wts = Wts Double Int deriving Show
worldToScreen :: Frab -> Wts
worldToScreen (Frab world (w, h)) = Wts scale translate
  where hMargin = screenWidth `div` 10
        vMargin = screenHeight `div` 10
        hScale :: Double
        hScale = (fromIntegral (screenWidth - 2 * hMargin)) / (fromIntegral (w + 2))
        vScale :: Double
        vScale = (fromIntegral (screenHeight - 2 * vMargin)) / (fromIntegral (h + 2))
        scale = min hScale vScale
        translate = min hMargin vMargin

toWorldCoordinate :: Wts -> Int -> Double
toWorldCoordinate (Wts s t) ix = ((fromIntegral ix) - (fromIntegral t)) / s

forDisplay :: Num a => Wts -> [Line a] -> [Line a]
forDisplay (Wts wtsScale wtsTranslate) lines = 
  translateLines (V2 (fromIntegral wtsTranslate) (fromIntegral wtsTranslate)) (scaleLines (fromIntegral (floor wtsScale)) lines)
forDisplayF :: Wts -> [Line Double] -> [Line Int]
forDisplayF wts lines = map floorL (forDisplay wts lines)
  where floorL (Line a b) = Line (floorV a) (floorV b)

horToLine x y = Line (V2 x y) (V2 (x + 1) y)
verToLine x y = Line (V2 x y) (V2 x (y + 1))

allWalls frab@(Frab _ (w, h)) = [horToLine x y | x <- [0..w], y <- [0..h], isHorWall frab x y] ++ [verToLine x y | x <- [0..w], y <- [0..h], isVerWall frab x y]

drawMap wts frab = drawLines map
  where map = forDisplay wts (allWalls frab) -- translateLines (V2 100 100) (scaleLines 50 allWalls)

data VStrip = VStrip Int Int Int Tex --deriving Show

--clipToScreen v | TR.trace (show v) False = undefined
clipToScreen (VStrip x y0 y1 tex) | y0 <= y1 =
  VStrip x (max 0 y0) (min (screenHeight - 1) y1) tex

fal xs = [head xs, last xs]

castRaysI frab frabT eye dirs = runST $
  do arr <- newArray (0, (length dirs)-1) Nothing :: ST s (STArray s Int (Maybe WallPt))
     let gorb (i, dir) = do
            writeArray arr i $ castRay frab frabT eye (signorm dir)
            return ()
     mapM_ (gorb) (zip [0..] dirs)
     getElems arr

-- Bool: is interpolated?
data Hit = Hit WallPt Bool

castRaysB frab frabT eye@(V2 ex ey) dirsL = runST $
  do arr <- newArray (0, last) Nothing :: ST s (STArray s Int (Maybe Hit))
     let  hab s e
            | s == e = return ()
            | s == e-1 = return ()
            | otherwise = do isSameHit <- sameHit s e
                             if isSameHit
                               then do interpolate s e
                               else let m = s + ((e - s) `div` 2)
                                     in do cast m
                                           hab s m
                                           hab m e
          cast i = writeArray arr i $ case castRay frab frabT eye (signorm (dirs ! i)) of Just wpt -> Just (Hit wpt False)
                                                                                          Nothing -> Nothing
          sameHit a b = do (Just (Hit aHit _)) <- readArray arr a
                           (Just (Hit bHit _)) <- readArray arr b
                           return $ sameWall aHit bHit
          interpolate s e = do
            (Just (Hit hit _)) <- readArray arr s
            case hit of (Hor fx y) -> mapM_ (hguvv y) [(s+1)..(e-1)]
                        (Ver x fy) -> mapM_ (vguvv x) [(s+1)..(e-1)]
            where hguvv wy i = writeArray arr i (Just iHit)
                    where (V2 dirx diry) = dirs ! i
                          dy = (fromIntegral wy) - ey
                          dx = dy * (dirx / diry)
                          hx = ex + dx
                          hy = wy
                          iHit = Hit (Hor hx hy) True
                  vguvv wx i = writeArray arr i (Just iHit)
                    where (V2 dirx diry) = dirs ! i
                          dx = (fromIntegral wx) - ex
                          dy = dx * (diry / dirx)
                          hx = wx
                          hy = ey + dy
                          iHit = Hit (Ver hx hy) True
                  correct i = castRay frab frabT eye (signorm (dirs ! i))
     cast 0
     cast last
     hab 0 last
     getElems arr
  where dirs = V.fromList dirsL
        last = (length dirs) - 1

renderWorld frab frabT worldTexMap eye ang ptr pitch = castAndShowL eye dirs ptr pitch
  where renderWall x eye hit dir ptr pitch = do
          case hit of
            Just h@(Hit hit interpolated) -> do
              let tex = getTexForHit frab h worldTexMap
              let hh = wallHalfScreenHeight eye eyeDir (wallPtToV2 hit)
              let unclippedVStrip = VStrip x ((screenHeight `div` 2) - hh) ((screenHeight `div` 2) + hh) tex
              fastestTextureVStripH (horPos hit) unclippedVStrip ptr pitch
            Nothing -> return ()
        hits = castRaysB frab frabT eye dirs
        castAndShowL eye dirs ptr pitch = do
          mapM_ (\(x, dir, hit) -> renderWall x eye hit dir ptr pitch) (zip3 [0..] dirs hits)
        wid = (screenWidth `div` 1)
        vpps = viewPlanePoints wid eye ang --(screenWidth `div` 2) eye ang
        dirs = map (\vpp -> signorm (vpp - eye)) vpps
        eyeDir = angToDir ang

drawEye wts eye ang ptr pitch = do
  drawLines (forDisplayF wts (boxAround eye)) ptr pitch
  drawLines (forDisplayF wts [eyeLine]) ptr pitch
  where eyeLine = Line eye (eye + angToDir ang)

drawAll wts frab frabT worldTexMap eye ang ptr pitch = do
  clearCanvas2 ptr pitch
  renderWorld frab frabT worldTexMap eye ang ptr pitch
  ifShowMap $ drawMap wts frab ptr pitch
  ifShowMap $ drawEye wts eye ang ptr pitch

fov = pi / 3
-- view plane starts one unit from origin perp to the x axis
viewPlaneWidth = 2.0 * tan (fov / 2)
viewPlaneLeft = V2 1.0 (viewPlaneWidth / 2)
viewPlaneRight = V2 1.0 (-(viewPlaneWidth / 2))
viewPlaneHeight = viewPlaneWidth * (fromIntegral screenHeight / fromIntegral screenWidth)

wallHalfHeight = 1.0

wallHalfScreenHeight :: V2 Double -> V2 Double -> V2 Double -> Int
--wallHalfScreenHeight eye dir hit | TR.trace (show (eye, dir, hit, ((hit - eye) `dot` dir), (norm (hit - eye)))) False = undefined
wallHalfScreenHeight eye eyeDir hit = screenHalfHeight
  where perpDist = (hit - eye) `dot` eyeDir
        hitViewPlaneHeight = wallHalfHeight /  perpDist
        screenHalfHeight = floor $ (fromIntegral (screenHeight `div` 2)) * (hitViewPlaneHeight / viewPlaneHeight)

angToDir ang = V2 (cos ang) (sin ang)
rotMatrix ang = V2 (V2 c (-s)) (V2 s c)
  where (V2 c s) = angToDir ang

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

viewPlanePoints :: Int -> V2 Double -> Double -> [V2 Double]
viewPlanePoints numPoints eye ang = lerpVVs rotatedViewPlane numPoints
  where rotatedViewPlane = ((multMV rM viewPlaneLeft) + eye, (multMV rM viewPlaneRight) + eye)
        rM = rotMatrix ang

dAng = 0.1
dMove = 0.1
updateEyeAng (eye, ang) keySet = (newEye, newAng)
  where newAng = if S.member (ord 'a') keySet
                   then ang + dAng
                   else if S.member (ord 'd') keySet
                          then ang - dAng
                          else  ang
        newEye = if S.member (ord 'w') keySet
                   then eye + (dMove * forwards)
                   else if S.member (ord 's') keySet
                          then eye - (dMove * forwards)
                          else eye
        forwards = multMV (rotMatrix ang) (V2 1.0 0.0)

readTexes :: IO (M.Map Char Tex)
readTexes = do
  start <- getCPUTime
  files <- listDirectory "images"
  let relativePaths = map ("images/" ++) files
  imgs <- mapM readImg relativePaths
  let darkImgs = map darkenImg imgs
  texes <- mapM imgToTex imgs
  darkTexes <- mapM imgToTex darkImgs
  let ret = M.fromList [(head file, tex) | (file, tex) <- zip files texes] `M.union`
            M.fromList [(toUpper $ head file, tex) | (file, tex) <- zip files darkTexes]
  end <- getCPUTime
  let dur = (fromIntegral (end - start)) / 1000000000000.0
  return ret

readMap :: String -> IO [[Char]]
readMap filename = do
  mapFile <- readFile filename
  return $ lines mapFile

readFrab :: String -> IO Frab
readFrab filename = do
  map <- readMap filename
  let worldSize = assert (allSameLength map)
                   ((length map), (length (map !! 0)))
  return $ Frab map worldSize

transposeFrab (Frab world (w, h)) = Frab (transposeAA world) (h, w)

screenToWorld wts (x, y) = (toWorldCoordinate wts x, toWorldCoordinate wts y)

processEvents :: Frab -> Wts -> KeySet -> (V2 Double) -> Double -> IO (KeySet, V2 Double, Double, Bool)
processEvents frab wts prevKeySet prevEye prevAng = do
  (cursorPos, newKeySet, quitEvent) <- getInput prevKeySet
  let (kEye, ang) = updateEyeAng (prevEye, prevAng) newKeySet
  let pEye = physics frab prevEye kEye
  let eye = case cursorPos of Just (x, y) -> case screenToWorld wts (x, y) of (x, y) -> (if outsideWorldF frab (V2 x y) then pEye else (V2 x y))
                              Nothing -> pEye
  let esc = S.member 27 newKeySet
  return (newKeySet, eye, ang, quitEvent || esc)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  --exitWith ExitSuccess
  frabT <- readFrab "map.txt"
  let frab = transposeFrab frabT
  texes <- readTexes
  let wts = worldToScreen frab

  let worldTexMap = WorldTexMap texes

  window <- windowInit screenWidth screenHeight

  startNow <- getPOSIXTime 

  let
    loop lastNow theta prevEye prevAng keySet frBuf = do
      (now, fps, newFRBuf) <- fps lastNow frBuf
      --putStrLn $ "FPS " ++ (show fps)

      (newKeySet, eye, ang, quit) <- processEvents frab wts keySet prevEye prevAng

      withFramebuffer window $ drawAll wts frab frabT worldTexMap eye ang

      blit window

      unless quit (loop now (theta + 2 `mod` 360) eye ang newKeySet newFRBuf)

  loop startNow (0 :: Int) (V2 40.5 7.5) (pi / 8) S.empty frBufferEmpty

  windowTerm window
