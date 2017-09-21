{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}

module Emulator.Nes (
    Nes(..)
  , Flag(..)
  , IncrementMode(..)
  , SpriteTableAddr(..)
  , BackgroundTableAddr(..)
  , SpriteSize(..)
  , ColorMode(..)
  , Visibility(..)
  , Address(..)
  , Cpu(..)
  , Ppu(..)
  , read
  , write
  , new
) where

import           Control.Monad.ST
import           Data.Bits                   (shiftL, shiftR, testBit, (.&.),
                                              (.|.))
import qualified Data.ByteString             as BS
import           Data.STRef
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Cartridge
import           Emulator.Util
import           Prelude                     hiding (cycles, read, replicate)

data IncrementMode = Horizontal | Vertical

data SpriteTableAddr = SpriteTable0000 | SpriteTable1000

data BackgroundTableAddr = BackgroundTable0000 | BackgroundTable1000

data SpriteSize = Normal | Double

data ColorMode = Color | Grayscale

data Visibility = Hidden | Shown

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

data PPU s = PPU {
  -- Misc
  ppuCycles             :: STRef s Int,
  scanline              :: STRef s Int,
  frameCount            :: STRef s Int,
  writeToggle           :: STRef s Bool,
  -- Data
  oamData               :: VUM.MVector s Word8,
  nameTableData         :: VUM.MVector s Word8,
  paletteData           :: VUM.MVector s Word8,
  screen                :: VUM.MVector s (Word8, Word8, Word8),
  -- Addresses
  currentVramAddress    :: STRef s Word16,
  tempVramAddress       :: STRef s Word16,
  oamAddress            :: STRef s Word8,
  -- Control register bits
  nameTable             :: STRef s Word16,
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

-- GADTs are used to represent addressing
data Cpu a where
  Pc:: Cpu Word16
  Sp:: Cpu Word8
  A :: Cpu Word8
  X :: Cpu Word8
  Y :: Cpu Word8
  P :: Cpu Word8
  CpuMemory8 :: Word16 -> Cpu Word8
  CpuMemory16 :: Word16 -> Cpu Word16
  CpuCycles :: Cpu Int

data Ppu a where
  PpuCycles :: Ppu Int
  Scanline :: Ppu Int
  FrameCount :: Ppu Int
  NameTableAddr :: Ppu Word16
  VBlank :: Ppu Bool
  PpuMemory8 :: Word16 -> Ppu Word8
  PpuMemory16 :: Word16 -> Ppu Word16
  Screen :: (Int, Int) -> Ppu (Word8, Word8, Word8)

data Address a where
  Cpu :: Cpu a -> Address a
  Ppu :: Ppu a -> Address a

data Flag
  = Negative
  | Overflow
  | Unused
  | Break
  | Decimal
  | Interrupt
  | Zero
  | Carry
  deriving (Enum)

new :: Cartridge -> ST s (Nes s)
new cart = do
  mapper <- pure $ mapper0 cart
  cpu <- newCPU
  ppu <- newPPU
  pure $ Nes cpu ppu mapper

read :: Nes s -> Address a -> ST s a
read nes addr = case addr of
  Cpu r -> readCPU nes r
  Ppu r -> readPPU nes r

write :: Nes s -> Address a -> a -> ST s ()
write nes addr v = case addr of
  Cpu r -> writeCPU nes r v
  Ppu r -> writePPU (ppu nes) r v

newCPU :: ST s (CPU s)
newCPU = do
  pc <- newSTRef 0x0
  sp <- newSTRef 0xFD
  a <- newSTRef 0x0
  x <- newSTRef 0x0
  y <- newSTRef 0x0
  p <- newSTRef 0x24 -- should this be 0x34?
  ram <- VUM.replicate 65536 0x0
  cycles <- newSTRef 0
  pure $ CPU pc sp a x y p ram cycles

writeCPU :: Nes s -> Cpu a -> a -> ST s ()
writeCPU nes addr v = case addr of
  Pc            -> modifySTRef' (pc $ cpu nes) (const v)
  Sp            -> modifySTRef' (sp $ cpu nes) (const v)
  A             -> modifySTRef' (a $ cpu nes) (const v)
  X             -> modifySTRef' (x $ cpu nes) (const v)
  Y             -> modifySTRef' (y $ cpu nes) (const v)
  P             -> modifySTRef' (p $ cpu nes) (const v)
  CpuCycles     -> modifySTRef' (cycles $ cpu nes) (const v)
  CpuMemory8 r  -> writeCpuMemory8 nes r v
  CpuMemory16 r -> writeCpuMemory16 nes r v

readCPU :: Nes s -> Cpu a -> ST s a
readCPU nes addr = case addr of
  Pc            -> readSTRef $ pc $ cpu nes
  Sp            -> readSTRef $ sp $ cpu nes
  A             -> readSTRef $ a $ cpu nes
  X             -> readSTRef $ x $ cpu nes
  Y             -> readSTRef $ y $ cpu nes
  P             -> readSTRef $ p $ cpu nes
  CpuCycles     -> readSTRef $ cycles $ cpu nes
  CpuMemory8 r  -> readCpuMemory8 nes r
  CpuMemory16 r -> readCpuMemory16 nes r

readCpuMemory8 :: Nes s -> Word16 -> ST s Word8
readCpuMemory8 nes addr
  | addr < 0x2000 = VUM.read (ram $ cpu nes) (fromIntegral addr `mod` 0x0800)
  | addr < 0x4000 = readPPURegister (ppu nes) addr
  | addr >= 0x4000 && addr <= 0x4017 = error "IO read not implemented!"
  | addr >= 0x4018 && addr <= 0x401F = error "APU read not implemented"
  | addr >= 0x6000 && addr <= 0xFFFF = pure $ readRom (mapper nes) addr
  | otherwise = error "Erroneous read detected!"

readCpuMemory16 :: Nes s -> Word16 -> ST s Word16
readCpuMemory16 nes addr = do
  lo <- readCpuMemory8 nes addr
  hi <- readCpuMemory8 nes (addr + 1)
  pure $ makeW16 lo hi

writeCpuMemory8 :: Nes s -> Word16 -> Word8 -> ST s ()
writeCpuMemory8 nes addr v
  | addr < 0x2000 = VUM.write (ram $ cpu nes) (fromIntegral addr `mod` 0x0800) v
  | addr < 0x4000 = writePPURegister nes addr v
  | addr >= 0x4000 && addr <= 0x4017 = pure ()
  | addr >= 0x4018 && addr <= 0x401F = error "APU write not implemented"
  | addr >= 0x4020 && addr <= 0xFFFF = error "Cannot write to cart space"
  | otherwise = error "Erroneous write detected!"

writeCpuMemory16 :: Nes s -> Word16 -> Word16 -> ST s ()
writeCpuMemory16 nes addr v = do
  let (lo, hi) = splitW16 v
  writeCpuMemory8 nes addr lo
  writeCpuMemory8 nes (addr + 1) lo

newPPU :: ST s (PPU s)
newPPU = do
  -- Misc
  cycles <- newSTRef 0
  scanline <- newSTRef 0
  frameCount <- newSTRef 0
  writeToggle <- newSTRef False
  -- Data
  oamData <- VUM.replicate 0x100 0x0
  nameTableData <- VUM.replicate 0x800 0x0
  paletteData <- VUM.replicate 0x20 0x0
  screen <- VUM.replicate (256 * 240) (0, 0, 0)
  -- Addresses
  currentVramAddress <- newSTRef 0x0
  tempVramAddress <- newSTRef 0x0
  oamAddress <- newSTRef 0x0
  -- Control register
  nameTable <- newSTRef 0x2000
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
    cycles scanline frameCount writeToggle
    -- Data
    oamData nameTableData paletteData screen
    -- Addresses
    currentVramAddress tempVramAddress oamAddress
    -- Control register
    nameTable incrementMode spriteTable bgTable spriteSize nmiEnabled
    -- Mask register
    colorMode leftBgVis leftSpritesVis bgVis spriteVis
    intensifyReds intensifyGreens intensifyBlues
    -- Status register
    lastWrite spriteOverflow spriteZeroHit vBlankStarted

readPPU :: Nes s -> Ppu a -> ST s a
readPPU nes addr = case addr of
  PpuCycles     -> readSTRef $ ppuCycles $ ppu nes
  NameTableAddr -> readSTRef $ nameTable $ ppu nes
  Scanline      -> readSTRef $ scanline $ ppu nes
  FrameCount    -> readSTRef $ frameCount $ ppu nes
  VBlank        -> readSTRef $ vBlank $ ppu nes
  Screen coords -> VUM.read (screen $ ppu nes) (translateXY coords 256)
  PpuMemory8 r  -> readPPUMemory nes r

writePPU :: PPU s -> Ppu a -> a -> ST s ()
writePPU ppu addr v = case addr of
  PpuCycles     -> modifySTRef' (ppuCycles ppu) (const v)
  Scanline      -> modifySTRef' (scanline ppu) (const v)
  FrameCount    -> modifySTRef' (frameCount ppu) (const v)
  VBlank        -> modifySTRef' (vBlank ppu) (const v)
  Screen coords -> VUM.write (screen ppu) (translateXY coords 256) v

readPPUMemory :: Nes s -> Word16 -> ST s Word8
readPPUMemory nes addr
  | addr < 0x2000 = error "Unimplemented cartridge read"
  | addr < 0x3F00 = VUM.read (nameTableData $ ppu nes) (fromIntegral $ addr' `mod` 0x800)
  | addr < 0x4000 = VUM.read (paletteData $ ppu nes) (fromIntegral $ addr' `mod` 0x20)
  | otherwise = error "Erroneous read detected!"
  where addr' = addr `mod` 0x4000

writePPUMemory :: Nes s -> Word16 -> Word8 -> ST s ()
writePPUMemory nes addr v
  | addr < 0x2000 = error "Unimplemented cartridge write"
  | addr < 0x3F00 = VUM.write (nameTableData $ ppu nes) (fromIntegral $ addr' `mod` 0x800) v
  | addr < 0x4000 = VUM.write (paletteData $ ppu nes) (fromIntegral $ addr' `mod` 0x20) v
  | otherwise = error "Erroneous write detected!"
  where addr' = addr `mod` 0x4000

readPPURegister :: PPU s -> Word16 -> ST s Word8
readPPURegister ppu addr = case (0x2000 + addr `mod` 8) of
  0x2002 -> readStatus ppu
  0x2004 -> readOAM ppu
  0x2007 -> readData ppu
  other  -> error $ "Unimplemented read at " ++ show addr

readStatus :: PPU s -> ST s Word8
readStatus ppu = do
  vBlankV <- readSTRef $ vBlank ppu
  let r = (fromEnum vBlankV) `shiftL` 7
  modifySTRef' (vBlank ppu) (const False)
  pure $ fromIntegral r

readOAM :: PPU s -> ST s Word8
readOAM ppu = error "Unimplemented PPU readOAM"

readData :: PPU s -> ST s Word8
readData ppu = error "Unimplemented PPU readData "

writePPURegister :: Nes s -> Word16 -> Word8 -> ST s ()
writePPURegister nes addr v = case (0x2000 + addr `mod` 8) of
  0x2000 -> writeControl (ppu nes) v
  0x2001 -> writeMask (ppu nes) v
  0x2003 -> writeOAMAddress (ppu nes) v
  0x2004 -> writeOAMData (ppu nes) v
  0x2005 -> writeScroll (ppu nes) v
  0x2006 -> writeAddress (ppu nes) v
  0x2007 -> writeData nes v
  0x4014 -> writeDMA nes v

writeControl :: PPU s -> Word8 -> ST s ()
writeControl ppu v = do
  modifySTRef' (nameTable ppu) $ const $ case (v `shiftR` 0) .&. 3 of
    0 -> 0x2000
    1 -> 0x2400
    2 -> 0x2800
    3 -> 0x2C00
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
        oamV <- readCpuMemory8 nes (toWord16 addr)
        VUM.write (oamData $ ppu nes) (toInt oamA) oamV
        modifySTRef' (oamAddress (ppu nes)) (+ 1)
        write nes (i + 1) (addr + 1)
      else
        pure ()

writeData :: Nes s -> Word8 -> ST s ()
writeData nes v = do
  addr <- readSTRef $ currentVramAddress (ppu nes)
  writePPUMemory nes addr v
  incMode <- readSTRef $ incrementMode (ppu nes)
  let inc = case incMode of
        Horizontal -> 1
        Vertical   -> 32
  modifySTRef' (currentVramAddress (ppu nes)) (+ inc)

translateXY :: (Int, Int) -> Int -> Int
translateXY (x, y) width = x + (y * width)
