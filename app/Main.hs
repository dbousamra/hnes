{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString              as BS
import           Data.Time.Clock.POSIX
import qualified Data.Vector.Storable.Mutable as VUM
import           Data.Word
import           Emulator                     (reset, step, stepCPU, stepFrame)
import           Emulator.Monad
import           Emulator.Nes
import           SDL                          as SDL
import           System.Environment           (getArgs)

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
  texture <- render' renderer
  copy renderer texture Nothing Nothing
  SDL.present renderer
  unless qPressed (appLoop renderer)


render' :: SDL.Renderer -> IOEmulator SDL.Texture
render' renderer = do
  mv <- load $ Ppu ScreenBuffer
  surface <- createRGBSurfaceFrom mv (V2 (256 * 1) (240 * 1)) (256 * 3) SDL.RGB24 -- RGB332
  texture <- createTextureFromSurface renderer surface
  SDL.freeSurface surface
  pure texture

currentTime :: IO Int
currentTime = round `fmap` getPOSIXTime
