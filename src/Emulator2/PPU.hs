module Emulator2.PPU (
    PPU(..)
  , new
  , step
) where

import           Data.IORef
import qualified Data.Vector.Storable.Mutable as VUM
import           Data.Word
import           Emulator2.CPU                (CPU)


data Interrupt
  = IRQ
  | NMI

data PPUMemory = PPUMemory
  { ram :: VUM.IOVector Word8
  }

data PPU = PPU
  { ppuCycles   :: IORef Int
  }

new :: IO PPU
new = do
  cycles <- newIORef 0
  pure $ PPU cycles

step :: PPU -> CPU -> IO ()
step ppu cpu = modifyIORef' (ppuCycles ppu) (+1)
