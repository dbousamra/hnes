{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Emulator               (reset, stepFrame)
import           Emulator.Monad
import           Emulator.Nes
import           SDL                    as SDL
import           System.Environment     (getArgs)

main2 :: IO ()
main2 = do
  cart' <- BS.readFile $ "roms/nestest.nes"
  runIOEmulator cart' $ do
    reset
    replicateM_ 1000 $ do
      _ <- stepFrame
      liftIO $ putStrLn "Stepped 1 frame"

main :: IO ()
main = do
  filename <- getArgs
  -- Set up SDL
  liftIO $ SDL.initializeAll

  let config = SDL.defaultWindow { windowInitialSize = V2 512 480 }
  window <- liftIO $ SDL.createWindow "hnes" config
  renderer <- liftIO $ SDL.createRenderer window (-1) SDL.defaultRenderer
  -- Create NES
  cart' <- BS.readFile $ head filename
  runIOEmulator cart' $ do
    reset
    appLoop renderer

appLoop :: SDL.Renderer -> IOEmulator ()
appLoop renderer = do
  _ <- stepFrame
  events <- liftIO $ SDL.pollEvents
  let eventIsQPress event =
        case eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  render renderer
  liftIO $ SDL.present renderer
  unless qPressed (appLoop renderer)

render :: SDL.Renderer -> IOEmulator ()
render renderer = do
  let scale = 2

  forM_ [0 .. 256 - 1] (\x ->
    forM_ [0 .. 240 - 1] (\y -> do
    let addr = (Ppu $ Screen (fromIntegral x, fromIntegral y))
    (r, g, b) <- load addr
    let color = V4 r g b maxBound
    SDL.rendererDrawColor renderer $= color
    let area = SDL.Rectangle
                  (SDL.P (V2 (x * scale) (y * scale)))
                  (V2 scale scale)
    SDL.fillRect renderer (Just area)))
