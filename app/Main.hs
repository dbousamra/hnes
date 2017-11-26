{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Data.Maybe             (catMaybes)
import qualified Data.Text              as T
import           Emulator               (reset, stepFrame)
import           Emulator.Controller    as Controller
import           Emulator.Monad
import           Emulator.Nes
import           SDL                    as SDL
import           SDL.Time
import           System.Environment     (getArgs)

main :: IO ()
main = do
  filename <- getArgs
  cart' <- BS.readFile $ head filename
  -- Set up SDL
  SDL.initializeAll
  -- Create Window
  let windowConfig = SDL.defaultWindow {
    windowInitialSize = V2 (fromIntegral $ width * scale) (fromIntegral $ height * scale)
  }
  window <- SDL.createWindow "hnes" windowConfig
  -- Create Renderer
  let rendererConfig = RendererConfig {
    rendererType          = AcceleratedRenderer,
    rendererTargetTexture = True
  }
  renderer <- SDL.createRenderer window (-1) rendererConfig
  -- Create NES
  runIOEmulator cart' $ do
    reset
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    stepFrame
    appLoop 0 0 renderer window

appLoop :: Double -> Int -> SDL.Renderer -> SDL.Window -> IOEmulator ()
appLoop lastTime frames renderer window = do
  intents <- liftIO $ eventsToIntents <$> SDL.pollEvents
  store Keys (intentsToKeys intents)
  -- stepFrame
  texture <- render renderer
  copy renderer texture Nothing Nothing
  SDL.present renderer

  time <- SDL.Time.time
  let diff = time - lastTime

  if (diff > 1.0) then do
    let fps = round $ fromIntegral frames / diff
    (windowTitle window) $= (T.pack $ "FPS = " ++ show fps)
    unless (elem Exit intents) (appLoop time 0 renderer window)
  else
    unless (elem Exit intents) (appLoop lastTime (frames + 1) renderer window)


render :: SDL.Renderer -> IOEmulator SDL.Texture
render renderer = do
  mv <- load $ Ppu ScreenBuffer
  surface <- createRGBSurfaceFrom mv (V2 256 240) (256 * fromIntegral scale) SDL.RGB24
  texture <- createTextureFromSurface renderer surface
  SDL.freeSurface surface
  pure texture

eventsToIntents :: [SDL.Event] -> [Intent]
eventsToIntents events = catMaybes $ eventToIntent . SDL.eventPayload <$> events
  where
    eventToIntent SDL.QuitEvent = Just Exit
    eventToIntent (SDL.KeyboardEvent k) = case k of
      (SDL.KeyboardEventData _ SDL.Pressed _ keysym) ->
        case SDL.keysymKeycode keysym of
          SDL.KeycodeQ      -> Just Exit
          SDL.KeycodeZ      -> Just (KeyPress Controller.A)
          SDL.KeycodeX      -> Just (KeyPress Controller.B)
          SDL.KeycodeUp     -> Just (KeyPress Controller.Up)
          SDL.KeycodeDown   -> Just (KeyPress Controller.Down)
          SDL.KeycodeLeft   -> Just (KeyPress Controller.Left)
          SDL.KeycodeRight  -> Just (KeyPress Controller.Right)
          SDL.KeycodeSpace  -> Just (KeyPress Controller.Select)
          SDL.KeycodeReturn -> Just (KeyPress Controller.Start)
          _                 -> Nothing
      _ -> Nothing
    eventToIntent _ = Nothing

intentsToKeys :: [Intent] -> [Controller.Key]
intentsToKeys = catMaybes . fmap (\x -> case x of
    KeyPress a -> Just a
    _          -> Nothing
  )

scale :: Int
scale = 3

width :: Int
width = 256

height :: Int
height = 240

data Intent
  = Exit
  | KeyPress Controller.Key
  deriving (Eq, Show)
