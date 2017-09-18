module Emulator.UI.SDL (
    render
) where

import           Control.Monad.IO.Class
import           Data.Vector.Unboxed    as V
import           Emulator.Address
import           Emulator.Monad
import           SDL                    as SDL

scale = 4

render :: (MonadIO m, MonadEmulator m) => SDL.Renderer -> m ()
render renderer = do
  screen <- load (PpuAddress Screen)
  liftIO $ putStrLn $ show $ V.length screen
  let color = V4 255 0 0 maxBound
  SDL.rendererDrawColor renderer $= color
  let area = SDL.Rectangle
               (SDL.P (V2 (1 * scale) (1 * scale)))
               (V2 scale scale)
  SDL.fillRect renderer (Just area)
