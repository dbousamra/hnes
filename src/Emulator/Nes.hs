{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emulator.Nes (
    Nes(..)
  , CPU(..)
  , PPU(..)
  , Emulator(..)
  , Sprite(..)
  , Coords
  , Color
  , Flag(..)
  , IncrementMode(..)
  , SpriteSize(..)
  , ColorMode(..)
  , Visibility(..)
  , Interrupt(..)
  , new
  , runEmulator
  , loadCpu
  , storeCpu
  , modifyCpu
  , loadPpu
  , storePpu
  , modifyPpu
  , readCpuMemory8
  , readCpuMemory16
  , writeCpuMemory8
  , writeCpuMemory16
  , readPpuMemory
  , readOAMData
  , readPalette
  , storeKeys
  , loadKeys
  , writeScreen
  , loadScreen
) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits                    (setBit, shiftL, shiftR, testBit, (.&.),
                                               (.|.))
import qualified Data.ByteString              as BS
import           Data.IORef
import           Data.Set                     as Set
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable.Mutable as VUM
import qualified Data.Vector.Unboxed          as VU
import           Data.Word
import           Emulator.Cartridge           as Cartridge
import qualified Emulator.Controller          as Controller
import qualified Emulator.Mapper              as Mapper
import           Emulator.Util
import           Prelude                      hiding (read, replicate)


import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader         (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.Trans          (MonadIO, lift)

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

data Interrupt
  = IRQ
  | NMI
  deriving (Eq, Show)

data Nes = Nes {
  cpu        :: CPU,
  ppu        :: PPU,
  cart       :: Cartridge,
  mapper     :: Mapper.Mapper,
  controller :: Controller.Controller
}

data CPU = CPU {
  pc        :: IORef Word16,
  sp        :: IORef Word8,
  a         :: IORef Word8,
  x         :: IORef Word8,
  y         :: IORef Word8,
  p         :: IORef Word8,
  cpuCycles :: IORef Int,
  interrupt :: IORef (Maybe Interrupt),
  ram       :: VUM.IOVector Word8
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

newtype Emulator a = Emulator { unNes :: ReaderT Nes IO a }
  deriving (Monad, Applicative, Functor, MonadIO, MonadReader Nes)

runEmulator :: BS.ByteString -> Emulator a ->  IO a
runEmulator bs (Emulator reader) = do
  cart <- Cartridge.parse bs
  nes <- new cart
  runReaderT reader nes

{-# INLINE with #-}
with :: (Nes -> b) -> (b -> IO a) -> Emulator a
with field f = do
  nes <- ask
  liftIO $ f (field nes)

{-# INLINE load #-}
load :: (Nes -> IORef b) -> Emulator b
load field = with field readIORef

{-# INLINE modify #-}
modify :: (Nes -> IORef b) -> (b -> b) -> Emulator ()
modify field v = with field (`modifyIORef'` v)

{-# INLINE loadCpu #-}
loadCpu :: (CPU -> IORef b) -> Emulator b
loadCpu field = load $ field . cpu

{-# INLINE storeCpu #-}
storeCpu :: (CPU -> IORef b) -> b -> Emulator ()
storeCpu field v = modify (field . cpu) (const v)

{-# INLINE modifyCpu #-}
modifyCpu :: (CPU -> IORef b) -> (b -> b) -> Emulator ()
modifyCpu field = modify (field . cpu)

{-# INLINE loadPpu #-}
loadPpu :: (PPU -> IORef b) -> Emulator b
loadPpu field = load $ field . ppu

{-# INLINE storePpu #-}
storePpu :: (PPU -> IORef b) -> b -> Emulator ()
storePpu field v = modify (field . ppu) (const v)

{-# INLINE modifyPpu #-}
modifyPpu :: (PPU -> IORef b) -> (b -> b) -> Emulator ()
modifyPpu field = modify (field . ppu)

readCpuMemory8 :: Word16 -> Emulator Word8
readCpuMemory8 addr
  | addr < 0x2000 = readCPURam addr
  | addr < 0x4000 = readPPURegister addr
  | addr == 0x4016 = readController
  | addr >= 0x4000 && addr <= 0x4017 = pure 0
  | addr >= 0x4018 && addr <= 0x401F = error "APU read not implemented"
  | addr >= 0x6000 = readMapper addr
  | otherwise = error "Erroneous read detected!"

readCpuMemory16 :: Word16 -> Emulator Word16
readCpuMemory16 addr = do
  lo <- readCpuMemory8 addr
  hi <- readCpuMemory8 (addr + 1)
  pure $ makeW16 lo hi

writeCpuMemory8 :: Word16 -> Word8 -> Emulator ()
writeCpuMemory8 addr value
  | addr < 0x2000 = writeCPURam addr value
  | addr < 0x4000 = writePPURegister (0x2000 + addr `mod` 8) value
  | addr == 0x4014 = writePPURegister addr value
  | addr == 0x4016 = writeController value
  | addr >= 0x4000 && addr <= 0x4017 = pure ()
  | addr >= 0x4018 && addr <= 0x401F = pure ()
  | addr >= 0x6000 = writeMapper addr value
  | otherwise = error "Erroneous write detected!"

writeCpuMemory16 :: Word16 -> Word16 -> Emulator ()
writeCpuMemory16 addr value = do
  let (lo, hi) = splitW16 value
  writeCpuMemory8 addr lo
  writeCpuMemory8 (addr + 1) hi

readPpuMemory :: Word16 -> Emulator Word8
readPpuMemory addr
  | addr' < 0x2000 = readMapper addr'
  | addr' < 0x3F00 = readNametableData addr'
  | addr' < 0x4000 = readPalette addr'
  | otherwise = error "Erroneous read detected!"
  where addr' = addr `mod` 0x4000

writePPUMemory :: Word16 -> Word8 -> Emulator ()
writePPUMemory addr v
  | addr' < 0x2000 = writeMapper addr' v
  | addr' < 0x3F00 = writeNametableData addr' v
  | addr' < 0x4000 = writePalette addr' v
  | otherwise = error "Erroneous write detected!"
  where addr' = addr `mod` 0x4000

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
  mapper <- Mapper.new cart
  controller <- Controller.new
  pure $ Nes cpu ppu cart mapper controller

newCPU :: IO CPU
newCPU = do
  pc <- newIORef 0x0
  sp <- newIORef 0xFD
  a <- newIORef 0x0
  x <- newIORef 0x0
  y <- newIORef 0x0
  p <- newIORef 0x24 -- should this be 0x34?
  cycles <- newIORef 0
  interrupt <- newIORef Nothing
  ram <- VUM.replicate 65536 0x0
  pure $ CPU pc sp a x y p cycles interrupt ram

readCPURam :: Word16 -> Emulator Word8
readCPURam addr = with cpu $ \cpu ->
  VUM.unsafeRead (ram cpu) (fromIntegral addr `mod` 0x0800)

writeCPURam :: Word16 -> Word8 -> Emulator ()
writeCPURam addr v = with cpu $ \cpu ->
  VUM.unsafeWrite (ram cpu) (fromIntegral addr `mod` 0x0800) v

readMapper :: Word16 -> Emulator Word8
readMapper addr = with mapper $ \mapper ->
  Mapper.read mapper addr

writeMapper :: Word16 -> Word8 -> Emulator ()
writeMapper addr value = with mapper $ \mapper ->
  Mapper.write mapper addr value

readController :: Emulator Word8
readController = with controller Controller.read

writeController :: Word8 -> Emulator ()
writeController value = with controller (`Controller.write` value)

storeKeys :: Set Controller.Key -> Emulator ()
storeKeys keys = with controller (`Controller.setKeysDown` keys)

loadKeys :: Emulator (Set Controller.Key)
loadKeys = with controller Controller.readKeysDown

writeNametableData :: Word16 -> Word8 -> Emulator ()
writeNametableData addr v = do
  mirror <- with cart $ \cart -> readIORef $ Cartridge.mirror cart
  let addr' = fromIntegral (mirroredNametableAddr addr mirror) `mod` 0x800
  with ppu $ \ppu ->
    VUM.unsafeWrite (nameTableData ppu) addr' v

readNametableData :: Word16 -> Emulator Word8
readNametableData addr = do
  mirror <- with cart $ \cart -> readIORef $ Cartridge.mirror cart
  let addr' = fromIntegral (mirroredNametableAddr addr mirror) `mod` 0x800
  with ppu $ \ppu ->
    VUM.unsafeRead (nameTableData ppu) addr'

writePalette :: Word16 -> Word8 -> Emulator ()
writePalette addr value = with ppu $ \ppu ->
  VUM.unsafeWrite (paletteData ppu) (fromIntegral $ mirroredPaletteAddr addr) value

readPalette :: Word16 -> Emulator Word8
readPalette addr = with ppu $ \ppu ->
  VUM.unsafeRead (paletteData ppu) (fromIntegral $ mirroredPaletteAddr addr)

readPPURegister :: Word16 -> Emulator Word8
readPPURegister addr = case 0x2000 + addr `mod` 8 of
  0x2002 -> readStatus
  0x2004 -> readOAMData'
  0x2007 -> readData
  other  -> error $ "Unimplemented read at " ++ show other

writePPURegister :: Word16 -> Word8 -> Emulator ()
writePPURegister addr v = case addr of
  0x2000 -> writeControl v
  0x2001 -> writeMask v
  0x2003 -> writeOAMAddress v
  0x2004 -> writeOAMData v
  0x2005 -> writeScroll v
  0x2006 -> writeAddress v
  0x2007 -> writeData v
  0x4014 -> writeDMA v

readStatus :: Emulator Word8
readStatus = do
  registerV <- loadPpu ppuRegister
  spriteOverflowV <- loadPpu spriteOverflow
  spriteZeroHitV <- loadPpu spriteZeroHit
  vBlankV <- loadPpu verticalBlank
  let r = registerV .&. 0x1F
  let r' = r .|. fromIntegral (fromEnum spriteOverflowV `shiftL` 5)
  let r'' = r' .|. fromIntegral (fromEnum spriteZeroHitV `shiftL` 6)
  let rFinal = r'' .|. fromIntegral (fromEnum vBlankV `shiftL` 7)
  storePpu verticalBlank False
  storePpu writeToggle False
  pure $ fromIntegral rFinal

readOAMData' :: Emulator Word8
readOAMData' = do
  addr <- loadPpu oamAddress
  with ppu $ \ppu ->
    VUM.unsafeRead (oamData ppu) (fromIntegral addr `mod` 0x0800)

readOAMData :: Word16 -> Emulator Word8
readOAMData addr = with ppu $ \ppu ->
  VUM.unsafeRead (oamData ppu) (fromIntegral addr)

readData :: Emulator Word8
readData = do
  addr <- loadPpu currentVramAddress

  rv <- if (addr `mod` 0x4000) < 0x3F00 then do
    v <- readPpuMemory addr
    buffered <- loadPpu dataV
    storePpu dataV v
    pure buffered
  else do
    v <- readPpuMemory (addr - 0x1000)
    storePpu dataV v
    readPpuMemory addr

  incMode <- loadPpu incrementMode
  let inc = case incMode of
        Horizontal -> 1
        Vertical   -> 32
  modifyPpu currentVramAddress (+ inc)
  pure rv

writeData :: Word8 -> Emulator ()
writeData v = do
  addr <- loadPpu currentVramAddress
  writePPUMemory addr v
  incMode <- loadPpu incrementMode
  let inc = case incMode of
        Horizontal -> 1
        Vertical   -> 32
  modifyPpu currentVramAddress (+ inc)

writeDMA :: Word8 -> Emulator ()
writeDMA v = do
  let startingAddr = toWord16 v `shiftL` 8
  let addresses = fmap (+ startingAddr) [0..255]
  forM_ addresses (\addr -> do
    oamA <- loadPpu oamAddress
    oamV <- readCpuMemory8 addr
    with ppu $ \ppu ->
      VUM.unsafeWrite (oamData ppu) (toInt oamA) oamV
    modifyPpu oamAddress (+ 1))

writeControl :: Word8 -> Emulator ()
writeControl v = do
  storePpu nameTable $ case (v `shiftR` 0) .&. 3 of
    0 -> 0x2000
    1 -> 0x2400
    2 -> 0x2800
    3 -> 0x2C00
  storePpu incrementMode $ if testBit v 2 then Vertical else Horizontal
  storePpu spriteTable $ if testBit v 3 then 0x1000 else 0x0000
  storePpu bgTable $ if testBit v 4 then 0x1000 else 0x0000
  storePpu spriteSize $ if testBit v 5 then Double else Normal
  storePpu nmiEnabled $ testBit v 7
  tv <- loadPpu tempVramAddress
  storePpu tempVramAddress ((tv .&. 0xF3FF) .|. (toWord16 v .&. 0x03) `shiftL` 10)

writeMask :: Word8 -> Emulator ()
writeMask v = do
  storePpu colorMode $ if testBit v 0 then Grayscale else Color
  storePpu leftBgVisibility $ if testBit v 1 then Shown else Hidden
  storePpu leftSpritesVisibility $ if testBit v 2 then Shown else Hidden
  storePpu bgVisibility $ testBit v 3
  storePpu spriteVisibility $ testBit v 4
  storePpu intensifyReds $ testBit v 5
  storePpu intensifyGreens $ testBit v 6
  storePpu intensifyBlues $ testBit v 7

writeOAMAddress :: Word8 -> Emulator ()
writeOAMAddress = storePpu oamAddress

writeOAMData :: Word8 -> Emulator ()
writeOAMData v = do
  addr <- loadPpu oamAddress
  with ppu $ \ppu ->
    VUM.unsafeWrite (oamData ppu) (toInt addr) v
  modifyPpu oamAddress (+ 1)

writeScroll :: Word8 -> Emulator ()
writeScroll v = do
  wv <- loadPpu writeToggle
  tv <- loadPpu tempVramAddress
  if wv then do
    let tv' = (tv .&. 0x8FFF) .|. ((toWord16 v .&. 0x07) `shiftL` 12)
    let tv'' = (tv' .&. 0xFC1F) .|. ((toWord16 v .&. 0xF8) `shiftL` 2)
    storePpu tempVramAddress tv''
    storePpu writeToggle False
  else do
    let tv' = (tv .&. 0xFFE0) .|. (toWord16 v `shiftR` 3)
    storePpu tempVramAddress tv'
    storePpu fineX $ v .&. 0x07
    storePpu writeToggle True

writeAddress :: Word8 -> Emulator ()
writeAddress v = do
  wv <- loadPpu writeToggle
  tv <- loadPpu tempVramAddress
  if wv then do
    let tv' = (tv .&. 0xFF00) .|. toWord16 v
    storePpu tempVramAddress tv'
    storePpu currentVramAddress tv'
    storePpu writeToggle False
  else do
    let tv' = (tv .&. 0x80FF) .|. ((toWord16 v .&. 0x3F) `shiftL` 8)
    storePpu tempVramAddress tv'
    storePpu writeToggle True

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

-- readPalette :: Word16 -> Emulator Word8
-- readPalette addr = with ppu $ \ppu ->
--   VUM.unsafeRead (paletteData ppu) addr
--   where addr = fromIntegral $ if (addr >= 16) && (addr `mod` 4 == 0) then addr - 16 else addr

mirroredPaletteAddr :: Word16 -> Word16
mirroredPaletteAddr addr = if addr' >= 16 && addr' `mod` 4 == 0 then addr' - 16 else addr'
  where addr' = addr `mod` 32

mirroredNametableAddr :: Word16 -> Mirror -> Word16
mirroredNametableAddr addr mirror = 0x2000 + lookup + offset
  where addr' = (addr - 0x2000) `mod` 0x1000
        tableIndex = fromIntegral $ addr' `div` 0x0400
        lookup = ((nameTableMirrorLookup V.! fromEnum mirror) V.! tableIndex) * 0x0400
        offset = fromIntegral $ addr' `mod` 0x0400

nameTableMirrorLookup :: V.Vector (V.Vector Word16)
nameTableMirrorLookup = V.fromList (fmap V.fromList [
    [0, 0, 1, 1],
    [0, 1, 0, 1],
    [0, 0, 0, 0],
    [1, 1, 1, 1],
    [0, 1, 2, 3]
  ])

loadScreen :: Emulator (VUM.IOVector Word8)
loadScreen = with ppu $ \ppu ->
  pure $ screen ppu

writeScreen :: Coords -> Color -> Emulator ()
writeScreen coords color = with ppu $ \ppu -> do
  let (r, g, b) = color
  let offset = fromIntegral $ translateXY coords 256 * 3
  VUM.write (screen ppu) (offset + 0) r
  VUM.write (screen ppu) (offset + 1) g
  VUM.write (screen ppu) (offset + 2) b

translateXY :: Coords -> Int -> Int
translateXY (x, y) width = x + (y * width)
