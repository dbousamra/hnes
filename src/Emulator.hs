module Emulator (
    step
  , stepFrame
  , reset
) where

import           Control.Monad
import           Control.Monad.Loops
import qualified Emulator.CPU        as CPU
import           Emulator.Monad
import           Emulator.Nes
import qualified Emulator.PPU        as PPU
import           Emulator.Trace      (Trace (..))
import           Prelude             hiding (and, compare)

step :: IOEmulator Trace
step = do
  (cycles', trace) <- CPU.step
  replicateM_ (cycles' * 3) PPU.step
  pure trace

stepFrame :: IOEmulator [Trace]
stepFrame = do
  frameCount <- load $ Ppu FrameCount
  untilM step $ do
    frameCount' <- load $ Ppu FrameCount
    pure $ frameCount' == (frameCount + 1)

reset :: IOEmulator ()
reset = CPU.reset >> PPU.reset

