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
  , Interrupt(..)
  , Address(..)
  , Cpu(..)
  , Ppu(..)
  , read
  , write
  , new
) where

import           Control.Monad.ST
import           Data.Bits                    (shiftL, shiftR, testBit, (.&.),
                                               (.|.))
import           Data.IORef
import qualified Data.Vector.Storable.Mutable as VUM
import qualified Data.Vector.Unboxed          as VU
import           Data.Word
import           Emulator.Cartridge
import           Emulator.Util
import           Prelude                      hiding (read, replicate)

data IncrementMode = Horizontal | Vertical

data SpriteTableAddr = SpriteTable0000 | SpriteTable1000

data BackgroundTableAddr = BackgroundTable0000 | BackgroundTable1000

data SpriteSize = Normal | Double

data ColorMode = Color | Grayscale

data Visibility = Hidden | Shown

data Nes = Nes {
  cpu  :: CPU,
  ppu  :: PPU,
  cart :: Cartridge
}

data Interrupt
  = IRQ
  | NMI
  deriving (Eq, Show)

data CPU = CPU {
  pc        :: IORef        Word16,
  sp        :: IORef        Word8,
  a         :: IORef        Word8,
  x         :: IORef        Word8,
  y         :: IORef        Word8,
  p         :: IORef        Word8,
  ram       :: VUM.IOVector Word8,
  cycles    :: IORef        Int,
  interrupt :: IORef        (Maybe Interrupt)
}

data PPU = PPU {
  -- Misc
  ppuCycles             :: IORef Int,
  scanline              :: IORef Int,
  frameCount            :: IORef Int,
  writeToggle           :: IORef Bool,
  -- Data
  oamData               :: VUM.IOVector Word8,
  nameTableData         :: VUM.IOVector Word8,
  paletteData           :: VUM.IOVector Word8,
  screen                :: VUM.IOVector Word8,
  -- Addresses
  currentVramAddress    :: IORef Word16,
  oamAddress            :: IORef Word8,
  -- Control register bits
  nameTable             :: IORef Word16,
  incrementMode         :: IORef IncrementMode,
  spriteTable           :: IORef SpriteTableAddr,
  bgTable               :: IORef BackgroundTableAddr,
  spriteSize            :: IORef SpriteSize,
  nmiEnabled            :: IORef Bool,
  -- Mask register bits
  colorMode             :: IORef ColorMode,
  leftBgVisibility      :: IORef Visibility,
  leftSpritesVisibility :: IORef Visibility,
  bgVisibility          :: IORef Visibility,
  spriteVisibility      :: IORef Visibility,
  intensifyReds         :: IORef Bool,
  intensifyGreens       :: IORef Bool,
  intensifyBlues        :: IORef Bool,
  -- Status register bits
  lastWrite             :: IORef Word8,
  spriteOverflow        :: IORef Bool,
  spriteZeroHit         :: IORef Bool,
  verticalBlank         :: IORef Bool,

  -- Scroll register
  scrollXY              :: IORef Word16
}

-- GADTs are used to represent addressing
data Cpu a where
  Pc :: Cpu Word16
  Sp :: Cpu Word8
  A :: Cpu Word8
  X :: Cpu Word8
  Y :: Cpu Word8
  P :: Cpu Word8
  Interrupt :: Cpu (Maybe Interrupt)
  CpuMemory8 :: Word16 -> Cpu Word8
  CpuMemory16 :: Word16 -> Cpu Word16
  CpuCycles :: Cpu Int

data Ppu a where
  PpuCycles :: Ppu Int
  Scanline :: Ppu Int
  FrameCount :: Ppu Int
  NameTableAddr :: Ppu Word16
  BackgroundTableAddr :: Ppu BackgroundTableAddr
  VerticalBlank :: Ppu Bool
  GenerateNMI :: Ppu Bool
  PaletteData :: Int -> Ppu Word8
  PpuMemory8 :: Word16 -> Ppu Word8
  PpuMemory16 :: Word16 -> Ppu Word16
  Screen :: (Int, Int) -> Ppu (Word8, Word8, Word8)
  ScreenBuffer :: Ppu (VUM.IOVector Word8)

data Address a where
  Cpu :: Cpu a -> Address a
  Ppu :: Ppu a -> Address a

data Flag
  = Negative
  | Overflow
  | Unused
  | Break
  | Decimal
  | InterruptDisable
  | Zero
  | Carry
  deriving (Enum)

new :: Cartridge -> IO Nes
new cart = do
  cpu <- newCPU
  ppu <- newPPU
  pure $ Nes cpu ppu cart

read :: Nes -> Address a -> IO a
read nes addr = case addr of
  Cpu r -> readCPU nes r
  Ppu r -> readPPU nes r

write :: Nes -> Address a -> a -> IO ()
write nes addr v = case addr of
  Cpu r -> writeCPU nes r v
  Ppu r -> writePPU (ppu nes) r v

newCPU :: IO CPU
newCPU = do
  pc <- newIORef 0x0
  sp <- newIORef 0xFD
  a <- newIORef 0x0
  x <- newIORef 0x0
  y <- newIORef 0x0
  p <- newIORef 0x24 -- should this be 0x34?
  ram <- VUM.replicate 65536 0x0
  cycles <- newIORef 0
  interrupt <- newIORef Nothing

  pure $ CPU pc sp a x y p ram cycles interrupt

writeCPU :: Nes -> Cpu a -> a -> IO ()
writeCPU nes addr v = case addr of
  Pc            -> modifyIORef' (pc $ cpu nes) (const v)
  Sp            -> modifyIORef' (sp $ cpu nes) (const v)
  A             -> modifyIORef' (a $ cpu nes) (const v)
  X             -> modifyIORef' (x $ cpu nes) (const v)
  Y             -> modifyIORef' (y $ cpu nes) (const v)
  P             -> modifyIORef' (p $ cpu nes) (const v)
  Interrupt     -> modifyIORef' (interrupt $ cpu nes) (const v)
  CpuCycles     -> modifyIORef' (cycles $ cpu nes) (const v)
  CpuMemory8 r  -> writeCpuMemory8 nes r v
  CpuMemory16 r -> writeCpuMemory16 nes r v

readCPU :: Nes -> Cpu a -> IO a
readCPU nes addr = case addr of
  Pc            -> readIORef $ pc $ cpu nes
  Sp            -> readIORef $ sp $ cpu nes
  A             -> readIORef $ a $ cpu nes
  X             -> readIORef $ x $ cpu nes
  Y             -> readIORef $ y $ cpu nes
  P             -> readIORef $ p $ cpu nes
  Interrupt     -> readIORef $ interrupt $ cpu nes
  CpuCycles     -> readIORef $ cycles $ cpu nes
  CpuMemory8 r  -> readCpuMemory8 nes r
  CpuMemory16 r -> readCpuMemory16 nes r

readCpuMemory8 :: Nes -> Word16 -> IO Word8
readCpuMemory8 nes addr
  | addr < 0x2000 = VUM.unsafeRead (ram $ cpu nes) (fromIntegral addr `mod` 0x0800)
  | addr < 0x4000 = readPPURegister (ppu nes) addr
  | addr >= 0x4000 && addr <= 0x4017 = pure 0
  | addr >= 0x4018 && addr <= 0x401F = error "APU read not implemented"
  | addr >= 0x6000 && addr <= 0xFFFF = readCart (cart nes) addr
  | otherwise = error "Erroneous read detected!"

readCpuMemory16 :: Nes -> Word16 -> IO Word16
readCpuMemory16 nes addr = do
  lo <- readCpuMemory8 nes addr
  hi <- readCpuMemory8 nes (addr + 1)
  pure $ makeW16 lo hi

writeCpuMemory8 :: Nes -> Word16 -> Word8 -> IO ()
writeCpuMemory8 nes addr v
  | addr < 0x2000 = VUM.unsafeWrite (ram $ cpu nes) (fromIntegral addr `mod` 0x0800) v
  | addr < 0x4000 = writePPURegister nes addr v
  | addr >= 0x4000 && addr <= 0x4017 = pure ()
  | addr >= 0x4018 && addr <= 0x401F = error "APU write not implemented"
  | addr >= 0x4020 && addr <= 0xFFFF = error "Cannot write to cart space"
  | otherwise = error "Erroneous write detected!"

writeCpuMemory16 :: Nes -> Word16 -> Word16 -> IO ()
writeCpuMemory16 nes addr v = do
  let (lo, hi) = splitW16 v
  writeCpuMemory8 nes addr lo
  writeCpuMemory8 nes (addr + 1) hi

newPPU :: IO PPU
newPPU = do
  -- Misc
  cycles <- newIORef 0
  scanline <- newIORef 0
  frameCount <- newIORef 0
  writeToggle <- newIORef False
  -- Data
  oamData <- VUM.replicate 0x100 0x0
  nameTableData <- VUM.replicate 0x800 0x0
  paletteData <- VUM.replicate 0x20 0x0
  screen <- VUM.replicate (256 * 240 * 3) 255
  -- Addresses
  currentVramAddress <- newIORef 0x0
  oamAddress <- newIORef 0x0
  -- Control register
  nameTable <- newIORef 0x2000
  incrementMode <- newIORef Horizontal
  spriteTable <- newIORef SpriteTable0000
  bgTable <- newIORef BackgroundTable0000
  spriteSize <- newIORef Normal
  nmiEnabled <- newIORef False
  -- Mask register
  colorMode <- newIORef Color
  leftBgVis <- newIORef Hidden
  leftSpritesVis <- newIORef Hidden
  bgVis <- newIORef Hidden
  spriteVis <- newIORef Hidden
  intensifyReds <- newIORef False
  intensifyGreens <- newIORef False
  intensifyBlues <- newIORef False
  -- Status register
  lastWrite <- newIORef 0x0
  spriteOverflow <- newIORef False
  spriteZeroHit <- newIORef False
  vBlankStarted <- newIORef False
  -- Scroll register
  scrollXY <- newIORef 0x0000

  pure $ PPU
    -- Misc
    cycles scanline frameCount writeToggle
    -- Data
    oamData nameTableData paletteData screen
    -- Addresses
    currentVramAddress oamAddress
    -- Control register
    nameTable incrementMode spriteTable bgTable spriteSize nmiEnabled
    -- Mask register
    colorMode leftBgVis leftSpritesVis bgVis spriteVis
    intensifyReds intensifyGreens intensifyBlues
    -- Status register
    lastWrite spriteOverflow spriteZeroHit vBlankStarted
    -- Scroll register
    scrollXY

readPPU :: Nes -> Ppu a -> IO a
readPPU nes addr = case addr of
  PpuCycles           -> readIORef $ ppuCycles $ ppu nes
  NameTableAddr       -> readIORef $ nameTable $ ppu nes
  Scanline            -> readIORef $ scanline $ ppu nes
  FrameCount          -> readIORef $ frameCount $ ppu nes
  VerticalBlank       -> readIORef $ verticalBlank $ ppu nes
  GenerateNMI         -> readIORef $ nmiEnabled $ ppu nes
  BackgroundTableAddr -> readIORef $ bgTable $ ppu nes
  PaletteData i       -> VUM.unsafeRead (paletteData $ ppu nes) i
  ScreenBuffer        -> pure $ screen $ ppu nes
  PpuMemory8 r        -> readPPUMemory nes r

writePPU :: PPU -> Ppu a -> a -> IO ()
writePPU ppu addr v = case addr of
  PpuCycles     -> modifyIORef' (ppuCycles ppu) (const v)
  Scanline      -> modifyIORef' (scanline ppu) (const v)
  FrameCount    -> modifyIORef' (frameCount ppu) (const v)
  VerticalBlank -> modifyIORef' (verticalBlank ppu) (const v)
  Screen coords -> do
    let (r, g, b) = v
    let offset = translateXY coords 256 * 3
    VUM.write (screen ppu) (offset + 0) r
    VUM.write (screen ppu) (offset + 1) g
    VUM.write (screen ppu) (offset + 2) b

readPPUMemory :: Nes -> Word16 -> IO Word8
readPPUMemory nes addr
  | addr' < 0x2000 = readCart (cart nes) addr'
  | addr' < 0x3F00 = VUM.unsafeRead (nameTableData $ ppu nes) (fromIntegral $ addr' `mod` 0x800)
  | addr' < 0x4000 = VUM.unsafeRead (paletteData $ ppu nes) (fromIntegral $ addr' `mod` 0x20)
  | otherwise = error "Erroneous read detected!"
  where addr' = addr `mod` 0x4000

writePPUMemory :: Nes -> Word16 -> Word8 -> IO ()
writePPUMemory nes addr v
  | addr' < 0x2000 = writeCart (cart nes) addr' v
  | addr' < 0x3F00 = VUM.unsafeWrite (nameTableData $ ppu nes) (fromIntegral $ addr' `mod` 0x800) v
  | addr' < 0x4000 = VUM.unsafeWrite (paletteData $ ppu nes) (fromIntegral $ addr' `mod` 0x20) v
  | otherwise = error "Erroneous write detected!"
  where addr' = addr `mod` 0x4000

readPPURegister :: PPU -> Word16 -> IO Word8
readPPURegister ppu addr = case 0x2000 + addr `mod` 8 of
  0x2002 -> readStatus ppu
  0x2004 -> readOAM ppu
  0x2007 -> readData ppu
  other  -> error $ "Unimplemented read at " ++ show other

readStatus :: PPU -> IO Word8
readStatus ppu = do
  vBlankV <- readIORef $ verticalBlank ppu
  let r = fromEnum vBlankV `shiftL` 7
  modifyIORef' (verticalBlank ppu) (const False)
  pure $ fromIntegral r

readOAM :: PPU -> IO Word8
readOAM ppu = error "Unimplemented PPU readOAM"

readData :: PPU -> IO Word8
readData ppu = error "Unimplemented PPU readData "

writePPURegister :: Nes -> Word16 -> Word8 -> IO ()
writePPURegister nes addr v = case 0x2000 + addr `mod` 8 of
  0x2000 -> writeControl (ppu nes) v
  0x2001 -> writeMask (ppu nes) v
  0x2003 -> writeOAMAddress (ppu nes) v
  0x2004 -> writeOAMData (ppu nes) v
  0x2005 -> writeScroll (ppu nes) v
  0x2006 -> writeAddress (ppu nes) v
  0x2007 -> writeData nes v
  0x4014 -> writeDMA nes v

writeControl :: PPU -> Word8 -> IO ()
writeControl ppu v = do
  modifyIORef' (nameTable ppu) $ const $ case (v `shiftR` 0) .&. 3 of
    0 -> 0x2000
    1 -> 0x2400
    2 -> 0x2800
    3 -> 0x2C00
  modifyIORef' (incrementMode ppu) $ const $ if testBit v 2 then Vertical else Horizontal
  modifyIORef' (spriteTable ppu) $ const $ if testBit v 3 then SpriteTable1000 else SpriteTable0000
  modifyIORef' (bgTable ppu) $ const $ if testBit v 4 then BackgroundTable1000 else BackgroundTable0000
  modifyIORef' (spriteSize ppu) $ const $ if testBit v 5 then Double else Normal
  modifyIORef' (nmiEnabled ppu) $ const $ testBit v 7

writeMask :: PPU -> Word8 -> IO ()
writeMask ppu v = do
  modifyIORef' (colorMode ppu) $ const $ if testBit v 0 then Grayscale else Color
  modifyIORef' (leftBgVisibility ppu) $ const $ if testBit v 1 then Shown else Hidden
  modifyIORef' (leftSpritesVisibility ppu) $ const $ if testBit v 2 then Shown else Hidden
  modifyIORef' (bgVisibility ppu) $ const $ if testBit v 3 then Shown else Hidden
  modifyIORef' (spriteVisibility ppu) $ const $ if testBit v 4 then Shown else Hidden
  modifyIORef' (intensifyReds ppu) $ const $ testBit v 5
  modifyIORef' (intensifyGreens ppu) $ const $ testBit v 6
  modifyIORef' (intensifyBlues ppu) $ const $ testBit v 7

writeOAMAddress :: PPU -> Word8 -> IO ()
writeOAMAddress ppu v = modifyIORef' (oamAddress ppu) (const v)

writeOAMData :: PPU -> Word8 -> IO ()
writeOAMData ppu v = error $ "Unimplemented writeOAMData at " ++ prettifyWord8 v

writeScroll :: PPU -> Word8 -> IO ()
writeScroll ppu v = do
  modifyIORef' (scrollXY ppu) (`shiftL` 8)
  modifyIORef' (scrollXY ppu) (.|. toWord16 v)

writeAddress :: PPU -> Word8 -> IO ()
writeAddress ppu v = do
  wt <- readIORef $ writeToggle ppu
  tVrV <- readIORef $ currentVramAddress ppu
  let v' = if wt then (tVrV .&. 0xFF00) .|. (toWord16 v) else (tVrV .&. 0x80FF) .|. (((toWord16 v) .&. 0x3F) `shiftL` 8)
  modifyIORef' (currentVramAddress ppu) (const v')
  modifyIORef' (writeToggle ppu) (const $ not wt)

writeDMA :: Nes -> Word8 -> IO ()
writeDMA nes v = do
  let startingAddr = toWord8 $ toWord16 v `shiftL` 8
  write nes 0 startingAddr
  where
    write :: Nes -> Int -> Word8 -> IO ()
    write nes i addr =
      if i < 255 then do
        oamA <- readIORef $ oamAddress (ppu nes)
        oamV <- readCpuMemory8 nes (toWord16 addr)
        VUM.unsafeWrite (oamData $ ppu nes) (toInt oamA) oamV
        modifyIORef' (oamAddress (ppu nes)) (+ 1)
        write nes (i + 1) (addr + 1)
      else
        pure ()

writeData :: Nes -> Word8 -> IO ()
writeData nes v = do
  addr <- readIORef $ currentVramAddress (ppu nes)
  writePPUMemory nes addr v
  incMode <- readIORef $ incrementMode (ppu nes)
  let inc = case incMode of
        Horizontal -> 1
        Vertical   -> 32
  modifyIORef' (currentVramAddress (ppu nes)) (+ inc)

translateXY :: (Int, Int) -> Int -> Int
translateXY (x, y) width = x + (y * width)
