{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}

module Emulator.Nes (
    Nes(..)
  , CPU(..)
  , PPU(..)
  , NameTableAddr(..)
  , IncrementMode(..)
  , SpriteTableAddr(..)
  , BackgroundTableAddr(..)
  , SpriteSize(..)
  , ColorMode(..)
  , Visibility(..)
  , load
  , store
  , new
) where

import           Control.Monad.ST
import           Data.Bits                   (shiftL, shiftR, testBit, (.&.),
                                              (.|.))
import qualified Data.ByteString             as BS
import           Data.STRef
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Address
import           Emulator.Cartridge
import           Emulator.Mapper
import           Emulator.Util
import           Prelude                     hiding (cycles, replicate)

data Nes s = Nes {
  cpu    :: CPU s,
  ppu    :: PPU s,
  mapper :: Mapper
}

data CPU s = CPU {
  pc     :: STRef       s Word16,
  sp     :: STRef       s Word8,
  a      :: STRef       s Word8,
  x      :: STRef       s Word8,
  y      :: STRef       s Word8,
  p      :: STRef       s Word8,
  ram    :: VUM.MVector s Word8,
  cycles :: STRef       s Int
}

data NameTableAddr = NameTable2000 | NameTable2400 | NameTable2800 | NameTable2C00

data IncrementMode = Horizontal | Vertical

data SpriteTableAddr = SpriteTable0000 | SpriteTable1000

data BackgroundTableAddr = BackgroundTable0000 | BackgroundTable1000

data SpriteSize = Normal | Double

data ColorMode = Color | Grayscale

data Visibility = Hidden | Shown

type Screen s = VUM.MVector s Word8

data PPU s = PPU {
  -- Misc
  ppuCycles             :: STRef s Int,
  scanline              :: STRef s Int,
  writeToggle           :: STRef s Bool,
  -- Data
  oamData               :: VUM.MVector s Word8,
  front                 :: Screen s,
  -- Addresses
  currentVramAddress    :: STRef s Word16,
  tempVramAddress       :: STRef s Word16,
  oamAddress            :: STRef s Word8,
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

new :: Cartridge -> ST s (Nes s)
new cart = do
  mapper <- pure $ mapper0 cart
  cpu <- newCPU
  ppu <- newPPU
  pure $ Nes cpu ppu mapper

load :: Nes s -> Address a -> ST s a
load nes addr = case addr of
  (CpuAddress r) -> readCPU (cpu nes) r
  (PpuAddress r) -> readPPU (ppu nes) r
  (Ram8 r)       -> readRam8 nes r
  (Ram16 r)      -> readRam16 nes r

store :: Nes s -> Address a -> a -> ST s ()
store nes addr v = case addr of
  (CpuAddress r) -> writeCPU (cpu nes) r v
  (PpuAddress r) -> writePPU (ppu nes) r v
  (Ram8 r)       -> writeRam8 nes r v
  (Ram16 r)      -> writeRam16 nes r v

readRam8 :: Nes s -> Word16 -> ST s Word8
readRam8 nes addr
  | addr < 0x2000 = VUM.read (ram $ cpu nes) (fromIntegral addr `mod` 0x0800)
  | addr < 0x4000 = readPPURegister (ppu nes) addr
  | addr >= 0x4000 && addr <= 0x4017 = error "IO read not implemented!"
  | addr >= 0x6000 && addr <= 0xFFFF = pure $ readRom (mapper nes) addr
  | otherwise = error "Erroneous read detected!"

readRam16 :: Nes s -> Word16 -> ST s Word16
readRam16 nes addr = do
  lo <- readRam8 nes addr
  hi <- readRam8 nes (addr + 1)
  pure $ makeW16 lo hi

writeRam8 :: Nes s -> Word16 -> Word8 -> ST s ()
writeRam8 nes r v
  | r < 0x2000 = VUM.write (ram $ cpu nes) (fromIntegral r `mod` 0x0800) v
  | r < 0x4000 = writePPURegister nes r v
  | r >= 0x4000 && r <= 0x4017 = pure ()
  | r >= 0x4020 && r <= 0xFFFF = error "Cannot write to cart space"
  | otherwise = error "Erroneous write detected!"

writeRam16 :: Nes s -> Word16 -> Word16 -> ST s ()
writeRam16 nes addr v = do
  let (lo, hi) = splitW16 v
  writeRam8 nes addr lo
  writeRam8 nes (addr + 1) lo

newCPU :: ST s (CPU s)
newCPU = do
  pc <- newSTRef 0x0
  sp <- newSTRef 0xFD
  a <- newSTRef 0x0
  x <- newSTRef 0x0
  y <- newSTRef 0x0
  p <- newSTRef 0x24 -- should this be 0x34?
  p <- newSTRef 0x24 -- should this be 0x34?
  ram <- VUM.replicate 65536 0x0
  cycles <- newSTRef 0
  pure $ CPU pc sp a x y p ram cycles

writeCPU :: CPU s -> CpuAddress a -> a -> ST s ()
writeCPU cpu addr v = case addr of
  Pc        -> modifySTRef' (pc cpu) (const v)
  Sp        -> modifySTRef' (sp cpu) (const v)
  A         -> modifySTRef' (a cpu) (const v)
  X         -> modifySTRef' (x cpu) (const v)
  Y         -> modifySTRef' (y cpu) (const v)
  P         -> modifySTRef' (p cpu) (const v)
  CpuCycles -> modifySTRef' (cycles cpu) (const v)

readCPU :: CPU s -> CpuAddress a -> ST s a
readCPU cpu addr = case addr of
  Pc        -> readSTRef $ pc cpu
  Sp        -> readSTRef $ sp cpu
  A         -> readSTRef $ a cpu
  X         -> readSTRef $ x cpu
  Y         -> readSTRef $ y cpu
  P         -> readSTRef $ p cpu
  CpuCycles -> readSTRef $ cycles cpu

newPPU :: ST s (PPU s)
newPPU = do
  -- Misc
  cycles <- newSTRef 0
  scanline <- newSTRef 0
  writeToggle <- newSTRef False
  -- Data
  oamData <- VUM.replicate 0x100 0x0
  front <- VUM.replicate (256 * 240) 0xAA
  -- Addresses
  currentVramAddress <- newSTRef 0x0
  tempVramAddress <- newSTRef 0x0
  oamAddress <- newSTRef 0x0
  -- Control regisyer
  nameTable <- newSTRef NameTable2000
  incrementMode <- newSTRef Horizontal
  spriteTable <- newSTRef SpriteTable0000
  bgTable <- newSTRef BackgroundTable0000
  spriteSize <- newSTRef Normal
  nmiEnabled <- newSTRef False
  -- Mask register
  colorMode <- newSTRef Color
  leftBgVis <- newSTRef Hidden
  leftSpritesVis <- newSTRef Hidden
  bgVis <- newSTRef Hidden
  spriteVis <- newSTRef Hidden
  intensifyReds <- newSTRef False
  intensifyGreens <- newSTRef False
  intensifyBlues <- newSTRef False
  -- Status register
  lastWrite <- newSTRef 0x0
  spriteOverflow <- newSTRef False
  spriteZeroHit <- newSTRef False
  vBlankStarted <- newSTRef False

  pure $ PPU
    -- Misc
    cycles scanline writeToggle
    -- Data
    oamData front
    -- Addresses
    currentVramAddress tempVramAddress oamAddress
    -- Control register
    nameTable incrementMode spriteTable bgTable spriteSize nmiEnabled
    -- Mask register
    colorMode leftBgVis leftSpritesVis bgVis spriteVis
    intensifyReds intensifyGreens intensifyBlues
    -- Status register
    lastWrite spriteOverflow spriteZeroHit vBlankStarted

readPPU :: PPU s -> PpuAddress a -> ST s a
readPPU ppu addr = case addr of
  PpuCycles -> readSTRef $ ppuCycles ppu
  Scanline  -> readSTRef $ scanline ppu
  VBlank    -> readSTRef $ vBlank ppu
  Screen    -> do
    V.freeze (front ppu)
    pure 1

writePPU :: PPU s -> PpuAddress a -> a -> ST s ()
writePPU ppu addr v = case addr of
  PpuCycles -> modifySTRef' (ppuCycles ppu) (const v)
  Scanline  -> modifySTRef' (scanline ppu) (const v)
  VBlank    -> modifySTRef' (vBlank ppu) (const v)

writePPURegister :: Nes s -> Word16 -> Word8 -> ST s ()
writePPURegister nes addr v = case (0x2000 + addr `mod` 8) of
  0x2000 -> writeControl (ppu nes) v
  0x2001 -> writeMask (ppu nes) v
  0x2003 -> writeOAMAddress (ppu nes) v
  0x2004 -> writeOAMData (ppu nes) v
  0x2005 -> writeScroll (ppu nes) v
  0x2006 -> writeAddress (ppu nes) v
  0x2007 -> writeDMA nes v

readPPURegister :: PPU s -> Word16 -> ST s Word8
readPPURegister ppu addr = case (0x2000 + addr `mod` 8) of
  0x2002 -> readStatus ppu
  0x2004 -> readOAM ppu
  0x2007 -> readMemory ppu

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

writeOAMAddress :: PPU s -> Word8 -> ST s ()
writeOAMAddress ppu v = error $ "Unimplemented writeOAMAddress at " ++ prettifyWord8 v

writeOAMData :: PPU s -> Word8 -> ST s ()
writeOAMData ppu v = error $ "Unimplemented writeOAMData at " ++ prettifyWord8 v

writeScroll :: PPU s -> Word8 -> ST s ()
writeScroll ppu v = error $ "Unimplemented writeScroll at " ++ prettifyWord8 v

writeAddress :: PPU s -> Word8 -> ST s ()
writeAddress ppu v = do
  wt <- readSTRef $ writeToggle ppu
  tVrV <- readSTRef $ tempVramAddress ppu
  let v' = case wt of
        False -> (tVrV .&. 0x80FF) .|. (((toWord16 v) .&. 0x3F) `shiftL` 8)
        True  -> (tVrV .&. 0xFF00) .|. (toWord16 v)
  modifySTRef' (tempVramAddress ppu) (const v')
  modifySTRef' (writeToggle ppu) (const $ not wt)

writeDMA :: Nes s -> Word8 -> ST s ()
writeDMA nes v = do
  let startingAddr = toWord8 $ toWord16 v `shiftL` 8
  write nes 0 startingAddr
  where
    write :: Nes s -> Int -> Word8 -> ST s ()
    write nes i addr =
      if i < 255 then do
        oamA <- readSTRef $ oamAddress (ppu nes)
        oamV <- readRam8 nes (toWord16 addr)
        VUM.write (oamData $ ppu nes) (toInt oamA) oamV
        modifySTRef' (oamAddress (ppu nes)) (+ 1)
        write nes (i + 1) (addr + 1)
      else
        pure ()

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
