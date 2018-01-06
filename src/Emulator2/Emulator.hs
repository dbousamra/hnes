module Emulator2.Emulator (
    new
  , step
) where

import           Control.Monad
import           Control.Monad.Loops
import qualified Emulator2.CPU       as CPU
import qualified Emulator2.PPU       as PPU

data Emulator = Emulator
  { cpu :: CPU.CPU
  , ppu :: PPU.PPU
  }

new :: IO Emulator
new = do
  cpu <- CPU.new
  ppu <- PPU.new
  pure $ Emulator cpu ppu

step :: Emulator -> IO ()
step (Emulator cpu ppu) = do
  c <- CPU.step cpu
  PPU.step ppu cpu
  s <- CPU.trace cpu
  putStrLn s

run :: IO ()
run = do
  emulator <- new
  replicateM_ 1000 (step emulator)
