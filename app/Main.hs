{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString     as BS
import           Data.Maybe          (catMaybes)
import           Data.Set            as Set hiding (foldl)
import qualified Data.Text           as T
import           Emulator            (reset, stepFrame)
import           Emulator.Controller as Controller
import           Emulator.Nes
import           SDL
import           SDL.Time
import           System.Environment  (getArgs)

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
  runEmulator cart' $ do
    reset
    appLoop 0 0 renderer window

appLoop :: Double -> Int -> SDL.Renderer -> SDL.Window -> Emulator ()
appLoop lastTime frames renderer window = do
  intents <- eventsToIntents <$> SDL.pollEvents
  oldKeys <- loadKeys
  storeKeys (intentsToKeys oldKeys intents)
  stepFrame
  texture <- render renderer
  copy renderer texture Nothing Nothing
  SDL.present renderer

  time <- SDL.Time.time
  let diff = time - lastTime

  if diff > 1.0 then do
    let fps = round $ fromIntegral frames / diff
    windowTitle window $= T.pack ("FPS = " ++ show fps)
    unless (Exit `elem`  intents) (appLoop time 0 renderer window)
  else
    unless (Exit `elem` intents) (appLoop lastTime (frames + 1) renderer window)

render :: SDL.Renderer -> Emulator SDL.Texture
render renderer = do
  mv <- loadScreen
  surface <- createRGBSurfaceFrom mv (V2 256 240) (256 * 3) SDL.RGB24
  texture <- createTextureFromSurface renderer surface
  SDL.freeSurface surface
  pure texture

eventsToIntents :: [SDL.Event] -> Set Intent
eventsToIntents events = Set.fromList $ catMaybes $ eventToIntent . SDL.eventPayload <$> events
  where
    eventToIntent :: SDL.EventPayload -> Maybe Intent
    eventToIntent SDL.QuitEvent = Just Exit
    eventToIntent (SDL.KeyboardEvent k) = case k of
      (SDL.KeyboardEventData _ motion _ keysym) -> do
        let c = case motion of
              SDL.Pressed  -> KeyPress
              SDL.Released -> KeyRelease
        case SDL.keysymKeycode keysym of
          SDL.KeycodeQ      -> Just Exit
          SDL.KeycodeZ      -> Just (c Controller.A)
          SDL.KeycodeX      -> Just (c Controller.B)
          SDL.KeycodeUp     -> Just (c Controller.Up)
          SDL.KeycodeDown   -> Just (c Controller.Down)
          SDL.KeycodeLeft   -> Just (c Controller.Left)
          SDL.KeycodeRight  -> Just (c Controller.Right)
          SDL.KeycodeSpace  -> Just (c Controller.Select)
          SDL.KeycodeReturn -> Just (c Controller.Start)
          _                 -> Nothing
    eventToIntent _ = Nothing

intentsToKeys :: Set Controller.Key -> Set Intent -> Set Controller.Key
intentsToKeys = foldl' op
  where op keys intent = case intent of
          KeyPress key   -> Set.insert key keys
          KeyRelease key -> Set.delete key keys
          Exit           -> keys

scale :: Int
scale = 2

width :: Int
width = 256

height :: Int
height = 240

data Intent
  = Exit
  | KeyPress Controller.Key
  | KeyRelease Controller.Key
  deriving (Eq, Show, Ord)
