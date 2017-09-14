{-# LANGUAGE GADTs #-}

module Emulator.PPU (
    PPU(..)
  , newPPU
  , write
  , read
) where

import           Control.Monad.ST
import           Data.Bits
import           Data.STRef
import           Data.Word
import           Debug.Trace
import           Emulator.Util
import           Prelude          hiding (read)

data NameTableAddr = NameTable2000 | NameTable2400 | NameTable2800 | NameTable2C00

data IncrementMode = Horizontal | Vertical

data SpriteTableAddr = SpriteTable0000 | SpriteTable1000

data BackgroundTableAddr = BackgroundTable0000 | BackgroundTable1000

data SpriteSize = Normal | Double

data ControlRegister = ControlRegister {
  nameTable     :: NameTableAddr,
  incrementMode :: IncrementMode,
  spriteTable   :: SpriteTableAddr,
  bgTable       :: BackgroundTableAddr,
  spriteSize    :: SpriteSize,
  nmiEnabled    :: Bool
}

data ColorMode = Color | Grayscale

data Visibility = Hidden | Shown

data MaskRegister = MaskRegister {
  colorMode             :: ColorMode,
  leftBgVisibility      :: Visibility,
  leftSpritesVisibility :: Visibility,
  bgVisibility          :: Visibility,
  spriteVisibility      :: Visibility,
  intensifyReds         :: Bool,
  intensifyGreens       :: Bool,
  intensifyBlues        :: Bool
}

data StatusRegister = StatusRegister {
  lastWrite      :: Word8,
  spriteOverflow :: Bool,
  spriteZeroHit  :: Bool,
  vBlankStarted  :: Bool
}

data PPU s = PPU {
  controlRegister :: STRef s ControlRegister,
  maskRegister    :: STRef s MaskRegister,
  statusRegister  :: STRef s StatusRegister
}

newPPU :: ST s (PPU s)
newPPU = do
  control <- newSTRef defaultControlRegister
  mask <- newSTRef defaultMaskRegister
  status <- newSTRef defaultStatusRegister
  pure $ PPU control mask status

write :: PPU s -> Word16 -> Word8 -> ST s ()
write ppu addr v = do
  status <- readStatus ppu
  let newStatus = status { lastWrite = v }
  case addr of
    0x2000 -> writeControl ppu v
    0x2001 -> writeMask ppu v

read :: PPU s -> Word16 -> ST s Word8
read ppu addr = case addr of
  0x2002 -> statusToWord8 <$> readStatus ppu
  0x2004 -> readOAM ppu
  0x2007 -> readMemory ppu

writeControl :: PPU s -> Word8 -> ST s ()
writeControl ppu v = modifySTRef' (controlRegister ppu) (const cr)
  where
    cr = ControlRegister ntV iaV stV bgtV ssV nmiV
    ntV = case (v `shiftR` 0) .&. 3 of
        0 -> NameTable2000
        1 -> NameTable2400
        2 -> NameTable2800
        3 -> NameTable2C00
    iaV = case testBit v 2 of
        False -> Horizontal
        True  -> Vertical
    stV = case testBit v 3 of
        False -> SpriteTable0000
        True  -> SpriteTable1000
    bgtV = case testBit v 4 of
        False -> BackgroundTable0000
        True  -> BackgroundTable1000
    ssV = case testBit v 5 of
        False -> Normal
        True  -> Double
    nmiV = testBit v 7

writeMask :: PPU s -> Word8 -> ST s ()
writeMask ppu v = modifySTRef' (maskRegister ppu) (const mr)
  where
    mr = MaskRegister cm lBgV lSV bgV sV iRV iGV iBV
    cm = case testBit v 0 of
        False -> Color
        True  -> Grayscale
    lBgV = case testBit v 1 of
        False -> Hidden
        True  -> Shown
    lSV = case testBit v 2 of
        False -> Hidden
        True  -> Shown
    bgV = case testBit v 3 of
        False -> Hidden
        True  -> Shown
    sV = case testBit v 4 of
        False -> Hidden
        True  -> Shown
    iRV = testBit v 5
    iGV = testBit v 6
    iBV = testBit v 7

readStatus :: PPU s -> ST s StatusRegister
readStatus ppu = readSTRef (statusRegister ppu)

readOAM :: PPU s -> ST s Word8
readOAM ppu = error $ "Unsupported PPU readOAM"

readMemory :: PPU s -> ST s Word8
readMemory ppu = error $ "Unsupported PPU readMemory "

-- TODO: FIX
statusToWord8 :: StatusRegister -> Word8
statusToWord8 (StatusRegister lw spOverflow spZeroHit vBStarted) =
  let start = lw
      av = if spOverflow then (setBit start 5) else (clearBit start 5)
      bv = if spZeroHit then (setBit start 6) else (clearBit start 6)
      cv = if vBStarted then (setBit start 7) else (clearBit start 7)
  in cv

defaultControlRegister :: ControlRegister
defaultControlRegister =
  ControlRegister {
    nameTable = NameTable2000,
    incrementMode = Horizontal,
    spriteTable = SpriteTable0000,
    bgTable = BackgroundTable0000,
    spriteSize = Normal,
    nmiEnabled = False
  }

defaultMaskRegister :: MaskRegister
defaultMaskRegister =
  MaskRegister {
    colorMode = Color,
    leftBgVisibility = Hidden,
    leftSpritesVisibility = Hidden,
    bgVisibility = Hidden,
    spriteVisibility = Hidden,
    intensifyReds = False,
    intensifyGreens = False,
    intensifyBlues = False
  }

defaultStatusRegister :: StatusRegister
defaultStatusRegister =
  StatusRegister {
    lastWrite = 0x0,
    spriteOverflow = False,
    spriteZeroHit = False,
    vBlankStarted = False
  }
