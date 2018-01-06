module Emulator2.Emulator (
    new
  , step
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import qualified Emulator2.CPU          as CPU
import qualified Emulator2.PPU          as PPU

data Emulator = Emulator
  { cpu :: CPU.CPU
  , ppu :: PPU.PPU
  }

new :: IO Emulator
new = do
  cpu <- CPU.new
  ppu <- PPU.new
  pure $ Emulator cpu ppu

step :: CPU.CPUEmulator ()
step = do
  c <- CPU.step
  -- PPU.step ppu cpu
  s <- CPU.trace
  liftIO $ putStrLn s

run :: IO ()
run = CPU.runCPUEmulator $ do
  replicateM_ 1000000 step
