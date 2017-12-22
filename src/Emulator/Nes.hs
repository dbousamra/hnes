{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}

module Emulator.Nes (
    Nes(..)
  , Sprite(..)
  , Coords
  , Color
  , Flag(..)
  , IncrementMode(..)
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

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits                    (setBit, shiftL, shiftR, testBit, (.&.),
                                               (.|.))
import           Data.IORef
import           Data.Set                     as Set
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable.Mutable as VUM
import qualified Data.Vector.Unboxed          as VU
import           Data.Word
import qualified Emulator.Cartridge           as Cartridge
import qualified Emulator.Controller          as Controller
import           Emulator.Util
import           Prelude                      hiding (read, replicate)

data Sprite = Sprite {
  sIndex         :: Int,
  sCoords        :: Coords,
  sTileIndexByte :: Word8,
  sAttributeByte :: Word8,
  sPattern       :: Word32,
  sPriority      :: Word8
} deriving (Show, Eq)

type Coords = (Int, Int)

type Color = (Word8, Word8, Word8)

data IncrementMode = Horizontal | Vertical

data SpriteSize = Normal | Double

data ColorMode = Color | Grayscale

data Visibility = Hidden | Shown

data Nes = Nes {
  cpu        :: CPU,
  ppu        :: PPU,
  cart       :: Cartridge.Cartridge,
  controller :: Controller.Controller
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
  ppuRegister           :: IORef Word8,
  -- Data
  oamData               :: VUM.IOVector Word8,
  nameTableData         :: VUM.IOVector Word8,
  paletteData           :: VUM.IOVector Word8,
  screen                :: VUM.IOVector Word8,
  -- Addresses
  currentVramAddress    :: IORef Word16,
  tempVramAddress       :: IORef Word16,
  oamAddress            :: IORef Word8,
  -- Control register bits
  nameTable             :: IORef Word16,
  incrementMode         :: IORef IncrementMode,
  spriteTable           :: IORef Word16,
  bgTable               :: IORef Word16,
  spriteSize            :: IORef SpriteSize,
  nmiEnabled            :: IORef Bool,
  -- Mask register bits
  colorMode             :: IORef ColorMode,
  leftBgVisibility      :: IORef Visibility,
  leftSpritesVisibility :: IORef Visibility,
  bgVisibility          :: IORef Bool,
  spriteVisibility      :: IORef Bool,
  intensifyReds         :: IORef Bool,
  intensifyGreens       :: IORef Bool,
  intensifyBlues        :: IORef Bool,
  -- Status register bits
  lastWrite             :: IORef Word8,
  spriteOverflow        :: IORef Bool,
  spriteZeroHit         :: IORef Bool,
  verticalBlank         :: IORef Bool,
  -- Scroll register
  fineX                 :: IORef Word8,
  -- Data register
  dataV                 :: IORef Word8,
  -- Temp vars
  nameTableByte         :: IORef Word8,
  attrTableByte         :: IORef Word8,
  loTileByte            :: IORef Word8,
  hiTileByte            :: IORef Word8,
  tileData              :: IORef Word64,
  sprites               :: IORef (V.Vector Sprite)
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
  CurrentVRamAddr :: Ppu Word16
  TempVRamAddr :: Ppu Word16
  BackgroundTableAddr :: Ppu Word16
  SpriteTableAddr :: Ppu Word16
  FineX :: Ppu Word8
  VerticalBlank :: Ppu Bool
  GenerateNMI :: Ppu Bool
  NameTableByte :: Ppu Word8
  AttrTableByte :: Ppu Word8
  LoTileByte :: Ppu Word8
  HiTileByte :: Ppu Word8
  SpriteSize :: Ppu SpriteSize
  BackgroundVisible :: Ppu Bool
  SpritesVisible :: Ppu Bool
  SpriteZeroHit :: Ppu Bool
  TileData :: Ppu Word64
  PaletteData :: Word16 -> Ppu Word8
  OamData :: Word16 -> Ppu Word8
  Sprites :: Ppu (V.Vector Sprite)
  PpuMemory8 :: Word16 -> Ppu Word8
  PpuMemory16 :: Word16 -> Ppu Word16
  Screen :: Coords -> Ppu Color
  ScreenBuffer :: Ppu (VUM.IOVector Word8)

data Address a where
  Cpu :: Cpu a -> Address a
  Ppu :: Ppu a -> Address a
  Keys :: Address (Set Controller.Key)

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

new :: Cartridge.Cartridge -> IO Nes
new cart = do
  cpu <- newCPU
  ppu <- newPPU
  controller <- Controller.new
  pure $ Nes cpu ppu cart controller

read :: Nes -> Address a -> IO a
read nes addr = case addr of
  Cpu r -> readCPU nes r
  Ppu r -> readPPU nes r
  Keys  -> readKeys nes

write :: Nes -> Address a -> a -> IO ()
write nes addr v = case addr of
  Cpu r -> writeCPU nes r v
  Ppu r -> writePPU (ppu nes) r v
  Keys  -> writeKeys nes v

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
  | addr < 0x2000 = readCPURam nes addr
  | addr < 0x4000 = readPPURegister nes addr
  | addr == 0x4016 = Controller.read $ controller nes
  | addr >= 0x4000 && addr <= 0x4017 = pure 0
  | addr >= 0x4018 && addr <= 0x401F = error "APU read not implemented"
  | addr >= 0x6000 && addr <= 0xFFFF = Cartridge.read (cart nes) addr
  | otherwise = error "Erroneous read detected!"

readCpuMemory16 :: Nes -> Word16 -> IO Word16
readCpuMemory16 nes addr = do
  lo <- readCpuMemory8 nes addr
  hi <- readCpuMemory8 nes (addr + 1)
  pure $ makeW16 lo hi

writeCpuMemory8 :: Nes -> Word16 -> Word8 -> IO ()
writeCpuMemory8 nes addr v
  | addr < 0x2000 = VUM.unsafeWrite (ram $ cpu nes) (fromIntegral addr `mod` 0x0800) v
  | addr < 0x4000 = writePPURegister nes (0x2000 + addr `mod` 8) v
  | addr == 0x4014 = writePPURegister nes addr v
  | addr == 0x4016 = Controller.write (controller nes) v
  | addr >= 0x4000 && addr <= 0x4017 = pure ()
  | addr >= 0x4018 && addr <= 0x401F = pure ()
  | addr >= 0x4020 && addr <= 0xFFFF = Cartridge.write (cart nes) addr v
  | otherwise = error "Erroneous write detected!"

writeCpuMemory16 :: Nes -> Word16 -> Word16 -> IO ()
writeCpuMemory16 nes addr v = do
  let (lo, hi) = splitW16 v
  writeCpuMemory8 nes addr lo
  writeCpuMemory8 nes (addr + 1) hi

readCPURam :: Nes -> Word16 -> IO Word8
readCPURam nes addr = VUM.unsafeRead (ram $ cpu nes) (fromIntegral addr `mod` 0x0800)

newPPU :: IO PPU
newPPU = do
  -- Misc
  cycles <- newIORef 0
  scanline <- newIORef 0
  frameCount <- newIORef 0
  writeToggle <- newIORef False
  ppuRegister <- newIORef 0x0
  -- Data
  oamData <- VUM.replicate 0x100 0x0
  nameTableData <- VUM.replicate 0x800 0x0
  paletteData <- VUM.replicate 0x20 0x0
  screen <- VUM.replicate (256 * 240 * 3) 255
  -- Addresses
  currentVramAddress <- newIORef 0x0
  tempVramAddress <- newIORef 0x0
  oamAddress <- newIORef 0x0
  -- Control register
  nameTable <- newIORef 0x2000
  incrementMode <- newIORef Horizontal
  spriteTable <- newIORef 0x0000
  bgTable <- newIORef 0x0000
  spriteSize <- newIORef Normal
  nmiEnabled <- newIORef False
  -- Mask register
  colorMode <- newIORef Color
  leftBgVis <- newIORef Hidden
  leftSpritesVis <- newIORef Hidden
  bgVis <- newIORef False
  spriteVis <- newIORef False
  intensifyReds <- newIORef False
  intensifyGreens <- newIORef False
  intensifyBlues <- newIORef False
  -- Status register
  lastWrite <- newIORef 0x0
  spriteOverflow <- newIORef False
  spriteZeroHit <- newIORef False
  vBlankStarted <- newIORef False
  -- Scroll register
  fineX <- newIORef 0x0
  -- Data register
  dataV <- newIORef 0x0
  -- Temp vars
  nameTableByte <- newIORef 0x0
  attrTableByte <- newIORef 0x0
  loTileByte <- newIORef 0x0
  hiTileByte <- newIORef 0x0
  tileData <- newIORef 0x0
  sprites <- newIORef V.empty

  pure $ PPU
    -- Misc
    cycles scanline frameCount writeToggle ppuRegister
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
    -- Scroll register
    fineX
    -- Data register
    dataV
    -- Temp vars
    nameTableByte attrTableByte loTileByte hiTileByte tileData sprites


readPPU :: Nes -> Ppu a -> IO a
readPPU nes addr = case addr of
  PpuCycles           -> readIORef $ ppuCycles $ ppu nes
  NameTableAddr       -> readIORef $ nameTable $ ppu nes
  CurrentVRamAddr     -> readIORef $ currentVramAddress $ ppu nes
  TempVRamAddr        -> readIORef $ tempVramAddress $ ppu nes
  Scanline            -> readIORef $ scanline $ ppu nes
  FrameCount          -> readIORef $ frameCount $ ppu nes
  FineX               -> readIORef $ fineX $ ppu nes
  VerticalBlank       -> readIORef $ verticalBlank $ ppu nes
  GenerateNMI         -> readIORef $ nmiEnabled $ ppu nes
  BackgroundTableAddr -> readIORef $ bgTable $ ppu nes
  SpriteTableAddr     -> readIORef $ spriteTable $ ppu nes
  NameTableByte       -> readIORef $ nameTableByte $ ppu nes
  BackgroundVisible   -> readIORef $ bgVisibility $ ppu nes
  SpritesVisible      -> readIORef $ spriteVisibility $ ppu nes
  SpriteZeroHit       -> readIORef $ spriteZeroHit $ ppu nes
  AttrTableByte       -> readIORef $ attrTableByte $ ppu nes
  LoTileByte          -> readIORef $ loTileByte $ ppu nes
  HiTileByte          -> readIORef $ hiTileByte $ ppu nes
  TileData            -> readIORef $ tileData $ ppu nes
  Sprites             -> readIORef $ sprites $ ppu nes
  SpriteSize          -> readIORef $ spriteSize $ ppu nes
  OamData addr        -> readOAMData' (ppu nes) addr
  PaletteData i       -> VUM.unsafeRead (paletteData $ ppu nes) (fromIntegral i)
  ScreenBuffer        -> pure $ screen $ ppu nes
  PpuMemory8 r        -> readPPUMemory nes r

writePPU :: PPU -> Ppu a -> a -> IO ()
writePPU ppu addr v = case addr of
  PpuCycles       -> modifyIORef' (ppuCycles ppu) (const v)
  Scanline        -> modifyIORef' (scanline ppu) (const v)
  FrameCount      -> modifyIORef' (frameCount ppu) (const v)
  CurrentVRamAddr -> modifyIORef' (currentVramAddress ppu) (const v)
  TempVRamAddr    -> modifyIORef' (tempVramAddress ppu) (const v)
  VerticalBlank   -> modifyIORef' (verticalBlank ppu) (const v)
  SpriteZeroHit   -> modifyIORef' (spriteZeroHit ppu) (const v)
  NameTableByte   -> modifyIORef' (nameTableByte ppu) (const v)
  AttrTableByte   -> modifyIORef' (attrTableByte ppu) (const v)
  LoTileByte      -> modifyIORef' (loTileByte ppu) (const v)
  HiTileByte      -> modifyIORef' (hiTileByte ppu) (const v)
  TileData        -> modifyIORef' (tileData ppu) (const v)
  Sprites         -> modifyIORef' (sprites ppu) (const v)
  Screen coords   -> do
    let (r, g, b) = v
    let offset = fromIntegral $ translateXY coords 256 * 3
    VUM.write (screen ppu) (offset + 0) r
    VUM.write (screen ppu) (offset + 1) g
    VUM.write (screen ppu) (offset + 2) b

readPPUMemory :: Nes -> Word16 -> IO Word8
readPPUMemory nes addr
  | addr' < 0x2000 = Cartridge.read (cart nes) addr'
  | addr' < 0x3F00 = readNametableData nes addr'
  | addr' < 0x4000 = readPalette nes addr'
  | otherwise = error "Erroneous read detected!"
  where addr' = addr `mod` 0x4000

writePPUMemory :: Nes -> Word16 -> Word8 -> IO ()
writePPUMemory nes addr v
  | addr' < 0x2000 = Cartridge.write (cart nes) addr' v
  | addr' < 0x3F00 = writeNametableData nes addr' v
  | addr' < 0x4000 = writePalette nes addr' v
  | otherwise = error "Erroneous write detected!"
  where addr' = addr `mod` 0x4000

readPPURegister :: Nes -> Word16 -> IO Word8
readPPURegister nes addr = case 0x2000 + addr `mod` 8 of
  0x2002 -> readStatus (ppu nes)
  0x2004 -> readOAMData (ppu nes)
  0x2007 -> readData nes
  other  -> error $ "Unimplemented read at " ++ show other

readStatus :: PPU -> IO Word8
readStatus ppu = do
  registerV <- readIORef $ ppuRegister ppu
  spriteOverflowV <- readIORef $ spriteOverflow ppu
  spriteZeroHitV <- readIORef $ spriteZeroHit ppu
  vBlankV <- readIORef $ verticalBlank ppu
  let r = registerV .&. 0x1F
  let r' = r .|. fromIntegral (fromEnum spriteOverflowV `shiftL` 5)
  let r'' = r' .|. fromIntegral (fromEnum spriteZeroHitV `shiftL` 6)
  let rFinal = r'' .|. fromIntegral (fromEnum vBlankV `shiftL` 7)
  modifyIORef' (verticalBlank ppu) (const False)
  modifyIORef' (writeToggle ppu) (const False)
  pure $ fromIntegral rFinal

readOAMData :: PPU -> IO Word8
readOAMData ppu = do
  addr <- readIORef $ oamAddress ppu
  VUM.unsafeRead (oamData ppu) (fromIntegral $ addr)

readOAMData' :: PPU -> Word16 -> IO Word8
readOAMData' ppu addr = VUM.unsafeRead (oamData ppu) (fromIntegral $ addr)

readData :: Nes -> IO Word8
readData nes = do
  addr <- readIORef $ currentVramAddress (ppu nes)

  rv <- if (addr `mod` 0x4000) < 0x3F00 then do
    v <- readPPUMemory nes addr
    buffered <- readIORef (dataV $ ppu nes)
    modifyIORef' (dataV $ ppu nes) (const v)
    pure buffered
  else do
    v' <- readPPUMemory nes (addr - 0x1000)
    modifyIORef' (dataV $ ppu nes) (const v')
    readPPUMemory nes addr

  incMode <- readIORef $ incrementMode (ppu nes)
  let inc = case incMode of
        Horizontal -> 1
        Vertical   -> 32
  modifyIORef' (currentVramAddress (ppu nes)) (+ inc)
  pure rv

writePPURegister :: Nes -> Word16 -> Word8 -> IO ()
writePPURegister nes addr v = case addr of
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
  modifyIORef' (spriteTable ppu) $ const $ if testBit v 3 then 0x1000 else 0x0000
  modifyIORef' (bgTable ppu) $ const $ if testBit v 4 then 0x1000 else 0x0000
  modifyIORef' (spriteSize ppu) $ const $ if testBit v 5 then Double else Normal
  modifyIORef' (nmiEnabled ppu) $ const $ testBit v 7
  tv <- readIORef (tempVramAddress ppu)
  modifyIORef' (tempVramAddress ppu) $ const ((tv .&. 0xF3FF) .|. (toWord16 v .&. 0x03) `shiftL` 10)

writeMask :: PPU -> Word8 -> IO ()
writeMask ppu v = do
  modifyIORef' (colorMode ppu) $ const $ if testBit v 0 then Grayscale else Color
  modifyIORef' (leftBgVisibility ppu) $ const $ if testBit v 1 then Shown else Hidden
  modifyIORef' (leftSpritesVisibility ppu) $ const $ if testBit v 2 then Shown else Hidden
  modifyIORef' (bgVisibility ppu) $ const $ testBit v 3
  modifyIORef' (spriteVisibility ppu) $ const $ testBit v 4
  modifyIORef' (intensifyReds ppu) $ const $ testBit v 5
  modifyIORef' (intensifyGreens ppu) $ const $ testBit v 6
  modifyIORef' (intensifyBlues ppu) $ const $ testBit v 7

writeOAMAddress :: PPU -> Word8 -> IO ()
writeOAMAddress ppu v = modifyIORef' (oamAddress ppu) (const v)

writeOAMData :: PPU -> Word8 -> IO ()
writeOAMData ppu v = do
  addr <- readIORef $ oamAddress ppu
  VUM.unsafeWrite (oamData ppu) (toInt addr) v
  modifyIORef' (oamAddress ppu) (+ 1)

writeScroll :: PPU -> Word8 -> IO ()
writeScroll ppu v = do
  wv <- readIORef (writeToggle ppu)
  tv <- readIORef (tempVramAddress ppu)
  if wv then do
    let tv' = (tv .&. 0x8FFF) .|. ((toWord16 v .&. 0x07) `shiftL` 12)
    let tv'' = (tv' .&. 0xFC1F) .|. ((toWord16 v .&. 0xF8) `shiftL` 2)
    modifyIORef' (tempVramAddress ppu) (const tv'')
    modifyIORef' (writeToggle ppu) (const False)
  else do
    let tv' = (tv .&. 0xFFE0) .|. (toWord16 v `shiftR` 3)
    modifyIORef' (tempVramAddress ppu) (const tv')
    modifyIORef' (fineX ppu) (const $ v .&. 0x07)
    modifyIORef' (writeToggle ppu) (const True)

writeAddress :: PPU -> Word8 -> IO ()
writeAddress ppu v = do
  wv <- readIORef $ writeToggle ppu
  tv <- readIORef $ tempVramAddress ppu
  if wv then do
    let tv' = (tv .&. 0xFF00) .|. (toWord16 v)
    modifyIORef' (tempVramAddress ppu) (const tv')
    modifyIORef' (currentVramAddress ppu) (const tv')
    modifyIORef' (writeToggle ppu) (const False)
  else do
    let tv' = (tv .&. 0x80FF) .|. (((toWord16 v) .&. 0x3F) `shiftL` 8)
    modifyIORef' (tempVramAddress ppu) (const tv')
    modifyIORef' (writeToggle ppu) (const True)

writeDMA :: Nes -> Word8 -> IO ()
writeDMA nes v = do
  let startingAddr = (toWord16 v) `shiftL` 8
  let addresses = fmap (+ startingAddr) [0..255]
  forM_ addresses (\addr -> do
    oamA <- readIORef $ oamAddress (ppu nes)
    oamV <- readCpuMemory8 nes addr
    VUM.unsafeWrite (oamData $ ppu nes) (toInt oamA) oamV
    modifyIORef' (oamAddress (ppu nes)) (+ 1))

writeData :: Nes -> Word8 -> IO ()
writeData nes v = do
  addr <- readIORef $ currentVramAddress (ppu nes)
  writePPUMemory nes addr v
  incMode <- readIORef $ incrementMode (ppu nes)
  let inc = case incMode of
        Horizontal -> 1
        Vertical   -> 32
  modifyIORef' (currentVramAddress (ppu nes)) (+ inc)

writeNametableData :: Nes -> Word16 -> Word8 -> IO ()
writeNametableData nes addr = VUM.unsafeWrite (nameTableData $ ppu nes) (fromIntegral $ addr' `mod` 0x800)
  where mirror = (Cartridge.mirror $ cart nes)
        addr' = addr

readNametableData :: Nes -> Word16 -> IO Word8
readNametableData nes addr = VUM.unsafeRead (nameTableData $ ppu nes) (fromIntegral $ addr' `mod` 0x800)
  where mirror = (Cartridge.mirror $ cart nes)
        addr' = mirroredNametableAddr addr mirror

writePalette :: Nes -> Word16 -> Word8 -> IO ()
writePalette nes addr = VUM.unsafeWrite (paletteData $ ppu nes) (fromIntegral $ mirroredPaletteAddr addr)

readPalette :: Nes -> Word16 -> IO Word8
readPalette nes addr = VUM.unsafeRead (paletteData $ ppu nes) (fromIntegral $ mirroredPaletteAddr addr)

mirroredPaletteAddr :: Word16 -> Word16
mirroredPaletteAddr addr = if addr' >= 16 && addr' `mod` 4 == 0 then addr' - 16 else addr'
  where addr' = addr `mod` 32

mirroredNametableAddr :: Word16 -> Int -> Word16
mirroredNametableAddr addr mirror = 0x2000 + lookup + offset
  where addr' = (addr - 0x2000) `mod` 0x1000
        tableIndex = fromIntegral $ addr' `div` 0x0400
        lookup = ((nameTableMirrorLookup V.! mirror) V.! tableIndex) * 0x4000
        offset = fromIntegral $ addr' `mod` 0x0400

nameTableMirrorLookup :: V.Vector (V.Vector Word16)
nameTableMirrorLookup = V.fromList (fmap V.fromList [
    [0, 0, 1, 1],
    [0, 1, 0, 1],
    [0, 0, 0, 0],
    [1, 1, 1, 1],
    [0, 1, 2, 3]
  ])

writeKeys :: Nes -> Set Controller.Key -> IO ()
writeKeys = Controller.setKeysDown . controller

readKeys :: Nes -> IO (Set Controller.Key)
readKeys = Controller.readKeysDown . controller

translateXY :: Coords -> Int -> Int
translateXY (x, y) width = x + (y * width)
