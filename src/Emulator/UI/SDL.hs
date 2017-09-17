module Emulator.UI.SDL (
    render
) where

import           Control.Monad.IO.Class
import           Emulator.Address
import           Emulator.Monad
import           SDL                    as SDL

render :: (MonadIO m, MonadEmulator m) => SDL.Renderer -> m ()
render r = do
  screen <- load (PpuAddress Screen)
  pure ()

