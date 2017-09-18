{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Emulator               (emulateDebug, reset, run, step)
import           Emulator.Cartridge     (parseCartridge)
import           Emulator.Monad         (MonadEmulator (..), runIOEmulator)
import           Emulator.Trace         (renderTrace)
import           Emulator.UI.SDL        (render)
import           SDL                    as SDL
import           System.Environment     (getArgs)

-- main :: IO ()
-- main = fmap head getArgs >>= run

main :: IO ()
main = do
  -- Set up SDL
  liftIO $ SDL.initializeAll
  window <- liftIO $ SDL.createWindow "hnes" SDL.defaultWindow
  renderer <- liftIO $ SDL.createRenderer window (-1) SDL.defaultRenderer
  -- Create NES
  cart <- parseCartridge <$> BS.readFile "roms/color_test.nes"
  runIOEmulator cart $ do
    reset
    appLoop renderer

appLoop :: (MonadIO m, MonadEmulator m) => SDL.Renderer -> m ()
appLoop renderer = do
  trace <- step
  liftIO $ putStrLn $ renderTrace trace

  events <- liftIO $ SDL.pollEvents
  let eventIsQPress event =
        case eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  liftIO $ SDL.clear renderer
  render renderer
  liftIO $ SDL.present renderer
  unless qPressed (appLoop renderer)
