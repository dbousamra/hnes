{-# LANGUAGE GADTs #-}

module Emulator.PPU (
    PPU(..)
  , newPPU
  , write
  , read
) where

import           Control.Monad.ST
import           Data.STRef
import           Data.Word
import           Debug.Trace
import           Emulator.Util
import           Prelude          hiding (read)

data NameTableAddr
  = NameTable2000
  | NameTable2400
  | NameTable2800
  | NameTable2C00

data IncrementMode
  = Horizontal
  | Vertical

data SpriteTableAddr
  = SpriteTable0000
  | SpriteTable1000

data BackgroundTableAddr
  = BackgroundTable0000
  | BackgroundTable1000

data SpriteSize
  = Normal
  | Double

data NMIEnabled
  = Off
  | On

data ControlRegister = ControlRegister {
  nameTable       :: NameTableAddr,
  incrementMode   :: IncrementMode,
  spriteTable     :: SpriteTableAddr,
  backgroundTable :: BackgroundTableAddr,
  spriteSize      :: SpriteSize,
  nmiEnabled      :: NMIEnabled
}

data PPU s = PPU {
  control :: STRef s ControlRegister
}

newPPU :: ST s (PPU s)
newPPU = do
  control <- newSTRef resetControlRegister
  pure $ PPU control

write :: PPU s -> Word16 -> Word8 -> ST s ()
write ppu addr v = case addr of
  0x2000 -> writeControl ppu addr v
  0x2001 -> writeMask ppu addr v

read :: PPU s -> Word16 -> ST s Word8
read ppu addr = case addr of
  0x2002 -> readStatus ppu
  0x2004 -> readOAM ppu
  0x2007 -> readMemory ppu

writeControl :: PPU s -> Word16 -> Word8 -> ST s ()
writeControl ppu addr v = error $ "Unsupported PPU writeControl"

writeMask :: PPU s -> Word16 -> Word8 -> ST s ()
writeMask ppu addr v = error $ "Unsupported PPU writeMask"

readStatus :: PPU s -> ST s Word8
readStatus ppu = error $ "Unsupported PPU readStatus"

readOAM :: PPU s -> ST s Word8
readOAM ppu = error $ "Unsupported PPU readOAM"

readMemory :: PPU s -> ST s Word8
readMemory ppu = error $ "Unsupported PPU readMemory "

resetControlRegister :: ControlRegister
resetControlRegister =
  ControlRegister {
    nameTable = NameTable2000,
    incrementMode = Horizontal,
    spriteTable = SpriteTable0000,
    backgroundTable = BackgroundTable0000,
    spriteSize = Normal,
    nmiEnabled = Off
  }
