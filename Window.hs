{-# LANGUAGE OverloadedStrings #-}
module Window
( Texture(..)
, blit
, windowInit
, windowTerm
) where

import Control.Monad hiding (mapM_)
import Data.Maybe
import Foreign.C.Types
import SDL (($=), unwrapKeycode, keysymKeycode, unwrapKeycode)
import SDL.Event
import qualified SDL.Raw.Types as RT
import SDL.Vect
import qualified SDL.Video.Renderer as VR
import qualified SDL

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

windowInit :: Int -> Int -> IO (SDL.Window, SDL.Renderer, Texture)
windowInit screenWidth screenHeight = do
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

  return (window, renderer, targetTexture)

blit renderer targetTexture screenWidth screenHeight = do
  let screenCenter = P (V2 (fromIntegral (screenWidth `div` 2)) (fromIntegral (screenHeight `div` 2)))
  setAsRenderTarget renderer Nothing
  renderTexture renderer targetTexture 0 Nothing (Just 0) (Just screenCenter) Nothing
  SDL.present renderer

windowTerm window = do
  SDL.destroyWindow window
  SDL.quit
