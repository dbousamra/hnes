module Emulator2.PPU (
    PPU(..)
  , new
  , step
) where

import           Data.IORef
import qualified Data.Vector.Storable.Mutable as VUM
import           Data.Word
import           Emulator2.Monad
import           Prelude                      hiding (cycle)


data Interrupt
  = IRQ
  | NMI

data PPUMemory = PPUMemory
  { ram :: VUM.IOVector Word8
  }

data PPU = PPU
  { cycle   :: IORef Int
  }

type PPUEmulator b = Emulator PPU b

new :: IO PPU
new = do
  cycles <- newIORef 0
  pure $ PPU cycles

step :: PPUEmulator Int
step = do
  modify cycle (+1)
  pure 1
