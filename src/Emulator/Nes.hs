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
import           Data.Bits                   (shiftL, shiftR, testBit, (.&.),
                                              (.|.))
import           Data.IORef
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Cartridge
import           Emulator.Util
import           Prelude                     hiding (read, replicate)

data IncrementMode = Horizontal | Vertical

data SpriteTableAddr = SpriteTable0000 | SpriteTable1000

data BackgroundTableAddr = BackgroundTable0000 | BackgroundTable1000

data SpriteSize = Normal | Double

data ColorMode = Color | Grayscale

data Visibility = Hidden | Shown

data Nes = Nes {
  cpu           :: IORef CPU,
  ppu           :: IORef PPU,
  cart          :: Cartridge,
  ram           :: VUM.MVector RealWorld Word8,
  oamData       :: VUM.MVector RealWorld Word8,
  nameTableData :: VUM.MVector RealWorld Word8,
  paletteData   :: VUM.MVector RealWorld Word8,
  screen        :: VUM.MVector RealWorld (Word8, Word8, Word8)
}

data Interrupt
  = IRQ
  | NMI
  deriving (Eq, Show)

data CPU = CPU {
  pc        :: Word16,
  sp        :: Word8,
  a         :: Word8,
  x         :: Word8,
  y         :: Word8,
  p         :: Word8,
  cycles    :: Int,
  interrupt :: Maybe Interrupt
}

data PPU = PPU {
  -- Misc
  ppuCycles             :: Int,
  scanline              :: Int,
  frameCount            :: Int,
  writeToggle           :: Bool,
  -- Addresses
  currentVramAddress    :: Word16,
  oamAddress            :: Word8,
  -- Control register bits
  nameTable             :: Word16,
  incrementMode         :: IncrementMode,
  spriteTable           :: SpriteTableAddr,
  bgTable               :: BackgroundTableAddr,
  spriteSize            :: SpriteSize,
  nmiEnabled            :: Bool,
  -- Mask register bits
  colorMode             :: ColorMode,
  leftBgVisibility      :: Visibility,
  leftSpritesVisibility :: Visibility,
  bgVisibility          :: Visibility,
  spriteVisibility      :: Visibility,
  intensifyReds         :: Bool,
  intensifyGreens       :: Bool,
  intensifyBlues        :: Bool,
  -- Status register bits
  lastWrite             :: Word8,
  spriteOverflow        :: Bool,
  spriteZeroHit         :: Bool,
  verticalBlank         :: Bool,
  -- Scroll register
  scrollXY              :: Word16
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
  cpu <- newIORef newCPU
  ppu <- newIORef newPPU
  -- CPU Memory
  ram <- VUM.replicate 65536 0x0
  -- PPU Memory
  oamData <- VUM.replicate 0x100 0x0
  nameTableData <- VUM.replicate 0x800 0x0
  paletteData <- VUM.replicate 0x20 0x0
  screen <- VUM.replicate (256 * 240) (0, 0, 0)
  pure $ Nes
    cpu ppu cart
    ram
    oamData nameTableData paletteData screen

read :: Nes -> Address a -> IO a
read nes addr = case addr of
  Cpu r -> readCPU nes r
  Ppu r -> readPPU nes r

write :: Nes -> Address a -> a -> IO ()
write nes addr v = case addr of
  Cpu r -> writeCPU nes r v
  Ppu r -> writePPU nes r v

newCPU :: CPU
newCPU = CPU pc sp a x y p cycles interrupt
  where
    pc = 0x0
    sp = 0xFD
    a = 0x0
    x = 0x0
    y = 0x0
    p = 0x24 -- should this be 0x34?
    cycles = 0
    interrupt = Nothing

writeCPU :: Nes -> Cpu a -> a -> IO ()
writeCPU nes addr v = case addr of
  Pc            -> do
    cpuv <- readIORef (cpu nes)
    modifyIORef' (cpu nes) (const $ cpuv { pc = v })
  Sp            -> do
    cpuv <- readIORef (cpu nes)
    modifyIORef' (cpu nes) (const $ cpuv { sp = v })
  A             -> do
    cpuv <- readIORef (cpu nes)
    modifyIORef' (cpu nes) (const $ cpuv { a = v })
  X             -> do
    cpuv <- readIORef (cpu nes)
    modifyIORef' (cpu nes) (const $ cpuv { x = v })
  Y             -> do
    cpuv <- readIORef (cpu nes)
    modifyIORef' (cpu nes) (const $ cpuv { y = v })
  P             -> do
    cpuv <- readIORef (cpu nes)
    modifyIORef' (cpu nes) (const $ cpuv { p = v })
  Interrupt     -> do
    cpuv <- readIORef (cpu nes)
    modifyIORef' (cpu nes) (const $ cpuv { interrupt = v })
  CpuCycles     -> do
    cpuv <- readIORef (cpu nes)
    modifyIORef' (cpu nes) (const $ cpuv { cycles = v })
  CpuMemory8 r  -> writeCpuMemory8 nes r v
  CpuMemory16 r -> writeCpuMemory16 nes r v


readCPU :: Nes -> Cpu a -> IO a
readCPU nes addr = case addr of
  Pc            -> pc <$> (readIORef $ cpu nes)
  Sp            -> sp <$> (readIORef $ cpu nes)
  A             -> a <$> (readIORef $ cpu nes)
  X             -> x <$> (readIORef $ cpu nes)
  Y             -> y <$> (readIORef $ cpu nes)
  P             -> p <$> (readIORef $ cpu nes)
  Interrupt     -> interrupt <$> (readIORef $ cpu nes)
  CpuCycles     -> cycles <$> (readIORef $ cpu nes)
  CpuMemory8 r  -> readCpuMemory8 nes r
  CpuMemory16 r -> readCpuMemory16 nes r

readCpuMemory8 :: Nes -> Word16 -> IO Word8
readCpuMemory8 nes addr
  | addr < 0x2000 = VUM.unsafeRead (ram nes) (fromIntegral addr `mod` 0x0800)
  | addr < 0x4000 = readPPURegister nes addr
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
  | addr < 0x2000 = VUM.unsafeWrite (ram  nes) (fromIntegral addr `mod` 0x0800) v
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

newPPU :: PPU
newPPU = let
    -- Misc
    cycles = 0
    scanline = 0
    frameCount = 0
    writeToggle = False
    -- Addresses
    currentVramAddress = 0x0
    oamAddress = 0x0
    -- Control register
    nameTable = 0x2000
    incrementMode = Horizontal
    spriteTable = SpriteTable0000
    bgTable = BackgroundTable0000
    spriteSize = Normal
    nmiEnabled = False
    -- Mask register
    colorMode = Color
    leftBgVis = Hidden
    leftSpritesVis = Hidden
    bgVis = Hidden
    spriteVis = Hidden
    intensifyReds = False
    intensifyGreens = False
    intensifyBlues = False
    -- Status register
    lastWrite = 0x0
    spriteOverflow = False
    spriteZeroHit = False
    vBlankStarted = False
    -- Scroll register
    scrollXY = 0x0000
  in
    PPU
      -- Misc
      cycles scanline frameCount writeToggle
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
  PpuCycles           -> ppuCycles <$> (readIORef $ ppu nes)
  NameTableAddr       -> nameTable <$> (readIORef $ ppu nes)
  Scanline            -> scanline <$> (readIORef $ ppu nes)
  FrameCount          -> frameCount <$> (readIORef $ ppu nes)
  VerticalBlank       -> verticalBlank <$> (readIORef $ ppu nes)
  GenerateNMI         -> nmiEnabled <$> (readIORef $ ppu nes)
  BackgroundTableAddr -> bgTable <$> (readIORef $ ppu nes)
  PaletteData i       -> VUM.unsafeRead (paletteData nes) i
  Screen coords       -> VUM.unsafeRead (screen nes) (translateXY coords 256)
  PpuMemory8 r        -> readPPUMemory nes r

writePPU :: Nes -> Ppu a -> a -> IO ()
writePPU nes addr v = case addr of
  PpuCycles            -> do
    ppuv <- readIORef (ppu nes)
    modifyIORef' (ppu nes) (const $ ppuv { ppuCycles = v })
  Scanline            -> do
    ppuv <- readIORef (ppu nes)
    modifyIORef' (ppu nes) (const $ ppuv { scanline = v })
  FrameCount            -> do
    ppuv <- readIORef (ppu nes)
    modifyIORef' (ppu nes) (const $ ppuv { frameCount = v })
  VerticalBlank            -> do
    ppuv <- readIORef (ppu nes)
    modifyIORef' (ppu nes) (const $ ppuv { verticalBlank = v })
  Screen coords -> VUM.unsafeWrite (screen nes) (translateXY coords 256) v

readPPUMemory :: Nes -> Word16 -> IO Word8
readPPUMemory nes addr
  | addr' < 0x2000 = readCart (cart nes) addr'
  | addr' < 0x3F00 = VUM.unsafeRead (nameTableData nes) (fromIntegral $ addr' `mod` 0x800)
  | addr' < 0x4000 = VUM.unsafeRead (paletteData nes) (fromIntegral $ addr' `mod` 0x20)
  | otherwise = error "Erroneous read detected!"
  where addr' = addr `mod` 0x4000

writePPUMemory :: Nes -> Word16 -> Word8 -> IO ()
writePPUMemory nes addr v
  | addr' < 0x2000 = writeCart (cart nes) addr' v
  | addr' < 0x3F00 = VUM.unsafeWrite (nameTableData nes) (fromIntegral $ addr' `mod` 0x800) v
  | addr' < 0x4000 = VUM.unsafeWrite (paletteData nes) (fromIntegral $ addr' `mod` 0x20) v
  | otherwise = error "Erroneous write detected!"
  where addr' = addr `mod` 0x4000

readPPURegister :: Nes -> Word16 -> IO Word8
readPPURegister nes addr = case 0x2000 + addr `mod` 8 of
  0x2002 -> readStatus nes
  0x2004 -> readOAM nes
  0x2007 -> readData nes
  other  -> error $ "Unimplemented read at " ++ show other

readStatus :: Nes -> IO Word8
readStatus nes = do
  ppuv <- readIORef $ ppu nes
  let r = fromEnum (verticalBlank ppuv) `shiftL` 7
  let newPpu = ppuv { verticalBlank = False }
  modifyIORef' (ppu nes) (const newPpu)
  pure $ fromIntegral r

readOAM :: Nes -> IO Word8
readOAM ppu = error "Unimplemented PPU readOAM"

readData :: Nes -> IO Word8
readData ppu = error "Unimplemented PPU readData "

writePPURegister :: Nes -> Word16 -> Word8 -> IO ()
writePPURegister nes addr v = case 0x2000 + addr `mod` 8 of
  0x2000 -> writeControl nes v
  0x2001 -> writeMask nes v
  0x2003 -> writeOAMAddress nes v
  0x2004 -> writeOAMData nes v
  0x2005 -> writeScroll nes v
  0x2006 -> writeAddress nes v
  0x2007 -> writeData nes v
  0x4014 -> writeDMA nes v

writeControl :: Nes -> Word8 -> IO ()
writeControl nes v = do
  ppuv <- readIORef $ ppu nes
  let newPpu = ppuv {
    nameTable = case (v `shiftR` 0) .&. 3 of
      0 -> 0x2000
      1 -> 0x2400
      2 -> 0x2800
      3 -> 0x2C00,
    incrementMode = if testBit v 2 then Vertical else Horizontal,
    spriteTable = if testBit v 3 then SpriteTable1000 else SpriteTable0000,
    bgTable = if testBit v 4 then BackgroundTable1000 else BackgroundTable0000,
    spriteSize = if testBit v 5 then Double else Normal,
    nmiEnabled = testBit v 7
  }
  modifyIORef' (ppu nes) (const newPpu)

writeMask :: Nes -> Word8 -> IO ()
writeMask nes v = do
  ppuv <- readIORef $ ppu nes
  let newPpu = ppuv {
    colorMode = if testBit v 0 then Grayscale else Color,
    leftBgVisibility = if testBit v 1 then Shown else Hidden,
    leftSpritesVisibility = if testBit v 2 then Shown else Hidden,
    bgVisibility = if testBit v 3 then Shown else Hidden,
    spriteVisibility = if testBit v 4 then Shown else Hidden,
    intensifyReds = testBit v 5,
    intensifyGreens = testBit v 6,
    intensifyBlues = testBit v 7
  }
  modifyIORef' (ppu nes) (const newPpu)

writeOAMAddress :: Nes -> Word8 -> IO ()
writeOAMAddress nes v = do
  ppuv <- readIORef $ ppu nes
  modifyIORef' (ppu nes) (const $ ppuv { oamAddress = v })

writeOAMData :: Nes -> Word8 -> IO ()
writeOAMData nes v = error $ "Unimplemented writeOAMData at " ++ prettifyWord8 v

writeScroll :: Nes -> Word8 -> IO ()
writeScroll nes v = do
  ppuv <- readIORef $ ppu nes
  let scrollXYv = scrollXY ppuv
  let newPpu = ppuv { scrollXY = (scrollXYv `shiftL` 8) .|. toWord16 v }
  modifyIORef' (ppu nes) (const newPpu)

writeAddress :: Nes -> Word8 -> IO ()
writeAddress nes v = do
  ppuv <- readIORef $ ppu nes
  let wt = writeToggle ppuv
  let tVrV = currentVramAddress ppuv
  let v' = if wt then (tVrV .&. 0xFF00) .|. (toWord16 v) else (tVrV .&. 0x80FF) .|. (((toWord16 v) .&. 0x3F) `shiftL` 8)
  let newPpu = ppuv { currentVramAddress = v', writeToggle = not wt }
  modifyIORef' (ppu nes) (const newPpu)

writeDMA :: Nes -> Word8 -> IO ()
writeDMA nes v = do
  let startingAddr = toWord8 $ toWord16 v `shiftL` 8
  write nes 0 startingAddr
  where
    write :: Nes -> Int -> Word8 -> IO ()
    write nes i addr =
      if i < 255 then do
        ppuv <- readIORef $ ppu nes
        let oamA = oamAddress ppuv
        oamV <- readCpuMemory8 nes (toWord16 addr)
        VUM.unsafeWrite (oamData nes) (toInt oamA) oamV
        let newPpu = ppuv { oamAddress = (oamAddress ppuv) + 1 }
        modifyIORef' (ppu nes) (const newPpu)
        write nes (i + 1) (addr + 1)
      else
        pure ()

writeData :: Nes -> Word8 -> IO ()
writeData nes v = do
  ppuv <- readIORef $ ppu nes
  let addr = currentVramAddress ppuv
  writePPUMemory nes addr v
  let inc = case incrementMode ppuv of
        Horizontal -> 1
        Vertical   -> 32
  let newPpu = ppuv { currentVramAddress = (currentVramAddress ppuv) + inc }
  modifyIORef' (ppu nes) (const newPpu)

translateXY :: (Int, Int) -> Int -> Int
translateXY (x, y) width = x + (y * width)
