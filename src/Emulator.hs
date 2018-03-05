module Emulator (
    step
  , stepFrame
  , reset
) where

import           Control.Monad
import           Control.Monad.Loops
import qualified Emulator.CPU        as CPU
import           Emulator.Nes
import qualified Emulator.PPU        as PPU
import           Emulator.Trace      (renderTrace)

step :: Emulator ()
step = do
  (cycles', t) <- CPU.stepT
  -- debug $ renderTrace t
  replicateM_ (cycles' * 3) PPU.step

stepFrame :: Emulator ()
stepFrame = do
  count <- loadPpu frameCount
  untilM_ step $ do
    count' <- loadPpu frameCount
    pure $ count' == (count + 1)

reset :: Emulator ()
reset = CPU.reset >> PPU.reset

