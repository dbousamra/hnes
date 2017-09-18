module Emulator.UI.SDL (
    render
) where

import           Control.Monad.IO.Class
import           Data.Vector.Unboxed    as V
import           Emulator.Address
import           Emulator.Monad
import           SDL                    as SDL

render :: (MonadIO m, MonadEmulator m) => SDL.Renderer -> m ()
render r = do
  screen <- load (PpuAddress Screen)
  liftIO $ putStrLn $ show $ V.length screen
  pure ()

