{-# LANGUAGE GADTs #-}

module Emulator.PPU (
    PPU(..)
  , newPPU
  , read
  , write
  , readRegister
  , writeRegister
) where

import           Control.Monad.ST
import           Data.Bits
import           Data.STRef
import           Data.Word
import           Debug.Trace
import           Emulator.Address
import           Emulator.Util
import           Prelude          hiding (read)

data NameTableAddr = NameTable2000 | NameTable2400 | NameTable2800 | NameTable2C00

data IncrementMode = Horizontal | Vertical

data SpriteTableAddr = SpriteTable0000 | SpriteTable1000

data BackgroundTableAddr = BackgroundTable0000 | BackgroundTable1000

data SpriteSize = Normal | Double

data ColorMode = Color | Grayscale

data Visibility = Hidden | Shown

data PPU s = PPU {
  cycles                :: STRef s Int,
  scanline              :: STRef s Int,
  -- Control register bits
  nameTable             :: STRef s NameTableAddr,
  incrementMode         :: STRef s IncrementMode,
  spriteTable           :: STRef s SpriteTableAddr,
  bgTable               :: STRef s BackgroundTableAddr,
  spriteSize            :: STRef s SpriteSize,
  nmiEnabled            :: STRef s Bool,
  -- Mask register bits
  colorMode             :: STRef s ColorMode,
  leftBgVisibility      :: STRef s Visibility,
  leftSpritesVisibility :: STRef s Visibility,
  bgVisibility          :: STRef s Visibility,
  spriteVisibility      :: STRef s Visibility,
  intensifyReds         :: STRef s Bool,
  intensifyGreens       :: STRef s Bool,
  intensifyBlues        :: STRef s Bool,
  -- Status register bits
  lastWrite             :: STRef s Word8,
  spriteOverflow        :: STRef s Bool,
  spriteZeroHit         :: STRef s Bool,
  vBlank                :: STRef s Bool
}

newPPU :: ST s (PPU s)
newPPU = do
  cycles <- newSTRef 0
  scanline <- newSTRef 0

  nameTable <- newSTRef NameTable2000
  incrementMode <- newSTRef Horizontal
  spriteTable <- newSTRef SpriteTable0000
  bgTable <- newSTRef BackgroundTable0000
  spriteSize <- newSTRef Normal
  nmiEnabled <- newSTRef False

  colorMode <- newSTRef Color
  leftBgVis <- newSTRef Hidden
  leftSpritesVis <- newSTRef Hidden
  bgVis <- newSTRef Hidden
  spriteVis <- newSTRef Hidden
  intensifyReds <- newSTRef False
  intensifyGreens <- newSTRef False
  intensifyBlues <- newSTRef False

  lastWrite <- newSTRef 0x0
  spriteOverflow <- newSTRef False
  spriteZeroHit <- newSTRef False
  vBlankStarted <- newSTRef False

  pure $ PPU
    cycles scanline nameTable
    -- Control
    incrementMode spriteTable bgTable spriteSize nmiEnabled
    -- Mask
    colorMode leftBgVis leftSpritesVis bgVis spriteVis
    intensifyReds intensifyGreens intensifyBlues
    -- Status
    lastWrite spriteOverflow spriteZeroHit vBlankStarted

read :: PPU s -> PpuAddress a -> ST s a
read ppu addr = case addr of
  PpuCycles -> readSTRef $ cycles ppu
  Scanline  -> readSTRef $ scanline ppu
  VBlank    -> readSTRef $ vBlank ppu

write :: PPU s -> PpuAddress a -> a -> ST s ()
write ppu addr v = case addr of
  PpuCycles -> modifySTRef' (cycles ppu) (const v)
  Scanline  -> modifySTRef' (scanline ppu) (const v)
  VBlank    -> modifySTRef' (vBlank ppu) (const v)

writeRegister :: PPU s -> Word16 -> Word8 -> ST s ()
writeRegister ppu addr v = case (0x2000 + addr `mod` 8) of
    0x2000 -> writeControl ppu v
    0x2001 -> writeMask ppu v

readRegister :: PPU s -> Word16 -> ST s Word8
readRegister ppu addr = case (0x2000 + addr `mod` 8) of
  0x2002 -> readStatus ppu
  -- 0x2004 -> readOAM ppu
  -- 0x2007 -> readMemory ppu

writeControl :: PPU s -> Word8 -> ST s ()
writeControl ppu v = do
  modifySTRef' (nameTable ppu) $ const $ case (v `shiftR` 0) .&. 3 of
    0 -> NameTable2000
    1 -> NameTable2400
    2 -> NameTable2800
    3 -> NameTable2C00
  modifySTRef' (incrementMode ppu) $ const $ case testBit v 2 of
    False -> Horizontal
    True  -> Vertical
  modifySTRef' (spriteTable ppu) $ const $ case testBit v 3 of
    False -> SpriteTable0000
    True  -> SpriteTable1000
  modifySTRef' (bgTable ppu) $ const $ case testBit v 4 of
    False -> BackgroundTable0000
    True  -> BackgroundTable1000
  modifySTRef' (spriteSize ppu) $ const $ case testBit v 5 of
    False -> Normal
    True  -> Double
  modifySTRef' (nmiEnabled ppu) $ const $ testBit v 7

writeMask :: PPU s -> Word8 -> ST s ()
writeMask ppu v = do
  modifySTRef' (colorMode ppu) $ const $ case testBit v 0 of
    False -> Color
    True  -> Grayscale
  modifySTRef' (leftBgVisibility ppu) $ const $ case testBit v 1 of
    False -> Hidden
    True  -> Shown
  modifySTRef' (leftSpritesVisibility ppu) $ const $ case testBit v 2 of
    False -> Hidden
    True  -> Shown
  modifySTRef' (bgVisibility ppu) $ const $ case testBit v 3 of
    False -> Hidden
    True  -> Shown
  modifySTRef' (spriteVisibility ppu) $ const $ case testBit v 4 of
    False -> Hidden
    True  -> Shown
  modifySTRef' (intensifyReds ppu) $ const $ testBit v 5
  modifySTRef' (intensifyGreens ppu) $ const $ testBit v 6
  modifySTRef' (intensifyBlues ppu) $ const $ testBit v 7

readStatus :: PPU s -> ST s Word8
readStatus ppu = do
  vBlankV <- readSTRef $ vBlank ppu
  let r = (fromEnum vBlankV) `shiftL` 7
  modifySTRef' (vBlank ppu) (const False)
  pure $ fromIntegral r

readOAM :: PPU s -> ST s Word8
readOAM ppu = error $ "Unsupported PPU readOAM"

readMemory :: PPU s -> ST s Word8
readMemory ppu = error $ "Unsupported PPU readMemory "

