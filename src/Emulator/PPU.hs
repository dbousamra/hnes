module Emulator.PPU (
    reset
  , step
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Emulator.Address
import           Emulator.Monad

reset :: MonadEmulator m => m ()
reset = pure ()

renderScanline :: (MonadIO m, MonadEmulator m) => m ()
renderScanline = liftIO $ putStrLn "Rendering scanline"

step :: (MonadIO m, MonadEmulator m) => m ()
step = do
  -- Update the counters, cycles etc
  tick

  scanline <- load (PpuAddress Scanline)
  cycles <- load (PpuAddress PpuCycles)

  -- Draw scanlines
  when (scanline < 240 && cycles == 1) $
    renderScanline

  -- Enter VBlank period
  when ((scanline == 241 && cycles == 1)) $
    store (PpuAddress VBlank) True

  -- Exit VBlank period
  when ((scanline == 261 && cycles == 1)) $
    store (PpuAddress VBlank) False

tick :: MonadEmulator m => m ()
tick = do
  modify (PpuAddress PpuCycles) (+1)
  cycles <- load $ PpuAddress PpuCycles

  when (cycles > 340) $ do
    modify (PpuAddress PpuCycles) (const 0)
    modify (PpuAddress Scanline) (+1)
    scanline <- load (PpuAddress Scanline)

    when (scanline > 261) $
      modify (PpuAddress Scanline) (const 0)

modify :: MonadEmulator m => Address a -> (a -> a) -> m ()
modify addr f = do
  av <- load addr
  store addr (f av)
