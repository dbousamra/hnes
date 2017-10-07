{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Data.Maybe             (catMaybes)
import           Emulator               (reset, stepFrame)
import           Emulator.Controller    as Controller
import           Emulator.Monad
import           Emulator.Nes
import           SDL                    as SDL
import           System.Environment     (getArgs)

main :: IO ()
main = do
  filename <- getArgs
  -- Set up SDL
  SDL.initializeAll
  let config = SDL.defaultWindow { windowInitialSize = V2 512 480 }
  window <- SDL.createWindow "hnes" config
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  -- Create NES
  cart' <- BS.readFile $ head filename
  runIOEmulator cart' $ do
    reset
    appLoop renderer

appLoop :: SDL.Renderer -> IOEmulator ()
appLoop renderer = do
  _ <- stepFrame
  intents <- liftIO $ eventsToIntents <$> SDL.pollEvents
  liftIO $ putStrLn (show intents)
  texture <- render renderer
  copy renderer texture Nothing Nothing
  SDL.present renderer
  unless (elem Exit intents) (appLoop renderer)


render :: SDL.Renderer -> IOEmulator SDL.Texture
render renderer = do
  mv <- load $ Ppu ScreenBuffer
  surface <- createRGBSurfaceFrom mv (V2 256 240) (256 * 3) SDL.RGB24
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
          SDL.KeycodeR      -> Just (KeyPress Controller.Reset)
          SDL.KeycodeSpace  -> Just (KeyPress Controller.Select)
          SDL.KeycodeReturn -> Just (KeyPress Controller.Start)
          _                 -> Nothing
      _ -> Nothing
    eventToIntent _ = Nothing

data Intent
  = Exit
  | KeyPress Controller.Key
  deriving (Eq, Show)
