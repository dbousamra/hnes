module Emulator.PPU.Execution (
    reset
  , step
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Emulator.Address
import           Emulator.Monad

reset :: MonadEmulator m => m ()
reset = pure ()

step :: (MonadIO m, MonadEmulator m) => m ()
step = do
  vb <- load $ PpuAddress VBlank
  -- liftIO $ putStrLn ("Stepping ppu. Cycles = " ++ show vb)
  modify (PpuAddress PpuCycles) (+1)
  cycles <- load $ PpuAddress PpuCycles
  if cycles > 340 then do
    modify (PpuAddress PpuCycles) (const 0)
    modify (PpuAddress Scanline) (+1)
    scanline <- load (PpuAddress Scanline)
    if scanline > 261 then
      modify (PpuAddress Scanline) (const 0)
    else
      pure ()
    pure ()
  else
    pure ()

  scanline <- load (PpuAddress Scanline)
  cycles <- load (PpuAddress PpuCycles)

  if (scanline == 241) && (cycles == 1) then
    store (PpuAddress VBlank) True
  else
    pure ()

  if (scanline == 261) && (cycles == 1) then
    store (PpuAddress VBlank) False
  else
    pure ()

modify :: MonadEmulator m => Address a -> (a -> a) -> m ()
modify addr f = do
  av <- load addr
  store addr (f av)
