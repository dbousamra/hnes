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

data ControlFlag
  = VBlank
  | SpriteHit
  | SpriteOverflow
  deriving (Enum)


data PPU s = PPU {
  control :: STRef       s Word8
}
data Address a where
  Control :: Address Word8

newPPU :: ST s (PPU s)
newPPU = do
  control <- newSTRef 0x0
  pure $ PPU control

write :: PPU s -> Word16 -> Word8 -> ST s ()
write ppu addr v = case addr of
  0x2000 -> writeControl ppu addr v
  0x2001 -> writeMask ppu addr v
  other  -> error $ "Unsupported write to PPU addr " ++ prettifyWord16 addr

writeControl :: PPU s -> Word16 -> Word8 -> ST s ()
writeControl ppu addr v = pure ()

writeMask :: PPU s -> Word16 -> Word8 -> ST s ()
writeMask ppu addr v = pure ()

read :: PPU s -> Word16 -> ST s Word8
read ppu addr = case addr of
  0x2002 -> readStatus ppu
  0x2004 -> readOAM ppu
  0x2007 -> readMemory ppu

readStatus :: PPU s -> ST s Word8
readStatus ppu = error $ "Unsupported PPU readStatus"

readOAM :: PPU s -> ST s Word8
readOAM ppu = error $ "Unsupported PPU readOAM"

readMemory :: PPU s -> ST s Word8
readMemory ppu = error $ "Unsupported PPU readMemory "
