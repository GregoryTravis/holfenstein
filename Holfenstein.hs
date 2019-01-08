{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Bits
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Word (Word32)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable (peekElemOff, pokeElemOff)
import Numeric (showHex)
import SDL.Vect
import SDL.Video.Renderer (lockTexture, unlockTexture)
import SDL (($=))
import qualified SDL
import System.IO

-- #if !MIN_VERSION_base(4,8,0)
-- import Control.Applicative
-- #endif

screenWidth, screenHeight :: CInt
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

colorFade x y =
  (shift red 24) .|. (shift green 16) .|. 0xff
  where red = floor $ 255 * ((fromIntegral x) / (fromIntegral screenWidth))
        green = floor $ 255 * ((fromIntegral y) / (fromIntegral screenHeight))

goof :: Texture -> IO ()
goof (Texture t _) = do
  (ptr, (CInt pitch)) <- lockTexture t Nothing
  let wordPtr :: Ptr Word32
      wordPtr = castPtr ptr
  let writeFade (x, y) = let off = y * ((fromIntegral pitch :: Int) `div` 4) + x
                          in pokeElemOff wordPtr off (colorFade x y)
  mapM_ writeFade [(x, y)
                      | x <- [0..((fromIntegral screenWidth :: Int)-1)]
                      , y <- [0..((fromIntegral screenHeight :: Int)-1)]]
  unlockTexture t

--data V2 = V2 Double Double
--data V3 = V3 Double Double Double

{-
-- Transform translate dir
data Transform = Transform V2 V2

makeTransform s ang = Transform s (cos ang, sin ang)
invertTransform (Transform s e) = Transform (invertTranslation s) (invertRotation e)
invertTranslation (V2 x y) = V2 (-x) (-y)
invertRotation (V2 x y) = 

verticalStrip screenX (eyeX, eyeY) (facingX, facingY) (hitX, hitY) =
  let eE = ((hitX - eyeX), (hitY - eyeY))
      projDist = dot eE (facingX, facingY)
   in projDist
  where dot (x0, y0) (x1, y1) = (x0 * x1) + (y0 * y1)
-}

data Vec2 = Vec2 Double Double deriving Show

newtype Angle = Angle Double deriving Show
data Translation = Translation Vec2 deriving Show
data Position = Position Vec2 deriving Show
newtype DirVec = DirVec Vec2 deriving Show

data ATransform = ATransform Angle Translation deriving Show
data VTransform = VTransform DirVec Translation deriving Show
data IVTransform = IVTransform DirVec Translation deriving Show

toVTransform :: ATransform -> VTransform
toVTransform (ATransform ang t) = VTransform (toDirVec ang) t
toIVTransform (ATransform ang t) = IVTransform (toDirVec ang) t

toDirVec (Angle ang) = DirVec $ Vec2 (cos ang) (sin ang)

invertAT (ATransform (Angle ang) t) = ATransform (Angle (- ang)) (invertT t)
invertT (Translation (Vec2 x y)) = Translation (Vec2 (-x) (-y))

--transformToView Position DirVec Position
--transformToView eye

transformPosition (VTransform dir t) (Position v2) =
  plusTV2 t (times dir v2)
iTransformPosition (IVTransform dir t) (Position v2) =
  --times dir $ minusV2T v2 t
  times dir $ plusTV2 t v2
times (DirVec (Vec2 dirX dirY)) v2 =
  plusV2 (timesSV2 dirX v2) (timesSV2 dirY (perpendicular v2))
-- Rotate pi/2
perpendicular (Vec2 x y) = Vec2 (- y) x
plusTV2 (Translation v) v2 = plusV2 v v2
minusV2T v2 (Translation v) = minusV2 v2 v
plusV2 (Vec2 x0 y0) (Vec2 x1 y1) = Vec2 (x0 + x1) (y0 + y1)
minusV2 (Vec2 x0 y0) (Vec2 x1 y1) = Vec2 (x0 - x1) (y0 - y1)
timesSV2 d (Vec2 x y) = Vec2 (d * x) (d * y)

posToTrans (Position v2) = Translation v2

vroo = do
  let eye = Position $ Vec2 3 3
  let hit = Position $ Vec2 6 6
  let ang = (pi :: Double) / 4.0
  --let inv = toVTransform $ invertAT $ ATransform (Angle ang) (posToTrans eye)
  let inv = toIVTransform $ invertAT $ ATransform (Angle ang) (posToTrans eye)
  let hitInView = iTransformPosition inv hit
  putStrLn $ show inv
  putStrLn $ show hitInView

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
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

  targetTexture <- createBlank renderer (V2 screenWidth screenHeight) SDL.TextureAccessStreaming
  goof targetTexture
  vroo

  let
    screenCenter = P (V2 (screenWidth `div` 2) (screenHeight `div` 2))

    loop theta = do
      --putStrLn $ "LOOP " ++ (show theta)
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
