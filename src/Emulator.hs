module Emulator (
    step
  , stepT
  , stepFrame
  , reset
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import qualified Emulator.CPU           as CPU
import           Emulator.Monad
import           Emulator.Nes
import qualified Emulator.PPU           as PPU
import           Emulator.Trace         (Trace (..))
import           Prelude                hiding (and, compare)

step :: IOEmulator ()
step = do
  cycles' <- CPU.step
  replicateM_ (cycles' * 3) PPU.step

stepT :: IOEmulator Trace
stepT = do
  (cycles', trace) <- CPU.stepT
  replicateM_ (cycles' * 3) PPU.step
  pure trace

stepFrame :: IOEmulator ()
stepFrame = do
  frameCount <- load $ Ppu FrameCount
  untilM_ step $ do
    frameCount' <- load $ Ppu FrameCount
    pure $ frameCount' == (frameCount + 1)

reset :: IOEmulator ()
reset = CPU.reset >> PPU.reset

