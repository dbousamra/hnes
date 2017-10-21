module Emulator.PPU (
    reset
  , step
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits              (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Vector            as V
import           Data.Word
import           Emulator.Monad
import           Emulator.Nes
import           Emulator.Util
import           Prelude                hiding (cycle)

data VBlankPhase
 = Enter
 | During
 | Exit
 deriving (Eq, Show)

data RenderPhase
  = Render
  | PostRender
  | VBlank VBlankPhase
  | Idle
  deriving (Eq, Show)

reset :: IOEmulator ()
reset = do
  store (Ppu PpuCycles) 340
  store (Ppu Scanline) 240
  store (Ppu VerticalBlank) False

step :: IOEmulator ()
step = do
  (scanline, cycle) <- tick

  let renderPhase = getRenderPhase scanline cycle

  case renderPhase of
    Render        -> renderTileRow scanline
    PostRender    -> idle
    VBlank Enter  -> enterVBlank
    VBlank During -> idle
    VBlank Exit   -> exitVBlank
    Idle          -> idle

tick :: IOEmulator (Int, Int)
tick = do
  modify (Ppu PpuCycles) (+1)
  cycles <- load $ Ppu PpuCycles

  when (cycles > 340) $ do
    store (Ppu PpuCycles) 0
    modify (Ppu Scanline) (+1)
    scanline <- load (Ppu Scanline)

    when (scanline > 261) $ do
      store (Ppu Scanline) 0
      modify (Ppu FrameCount) (+1)

  scanline <- load $ Ppu Scanline
  cycles <- load $ Ppu PpuCycles

  pure (scanline, cycles)

getRenderPhase :: Int -> Int -> RenderPhase
getRenderPhase scanline cycle
  | scanline >= 0 && scanline < 240 && cycle == 1 = Render
  | scanline == 240 = PostRender
  | scanline == 241 && cycle == 1 = VBlank Enter
  | scanline > 241 && scanline < 261 = VBlank During
  | scanline == 261 && cycle == 1 = VBlank Exit
  | otherwise = Idle

idle :: IOEmulator ()
idle = pure ()

enterVBlank :: IOEmulator ()
enterVBlank = do
  store (Ppu VerticalBlank) True
  generateNMI <- load (Ppu GenerateNMI)
  when generateNMI $ store (Cpu Interrupt) (Just NMI)

exitVBlank :: IOEmulator ()
exitVBlank = store (Ppu VerticalBlank) False

renderTileRow :: Int -> IOEmulator ()
renderTileRow scanline = do
  -- liftIO $ putStrLn $ "In render" ++ show scanline
  sx <- fromIntegral <$> load (Ppu ScrollX)
  sy <- fromIntegral <$> load (Ppu ScrollY)

  (y, nametable) <- do
    let y = sy + scanline
    nametable <- load (Ppu NameTableAddr)
    if y >= 240
      then (pure (y - 240, nametable + 2))
      else pure (y, nametable)


  let nameTable1 = (nametable + 0) `mod` 4
  let nameTable2 = (nametable + 1) `mod` 4

  line1 <- renderNameTableLine nameTable1 y
  line2 <- renderNameTableLine nameTable2 y

  let line = line1 V.++ line2

  forM_ [0 .. 255] (\i -> do
    let index = fromIntegral $ line V.! (sx + i)
    let color = palette V.! index
    let addr = Ppu $ Screen (i, scanline)
    store addr color
    )


renderNameTableLine :: Word16 -> Int -> IOEmulator (V.Vector Word8)
renderNameTableLine nametable y = do
  let ty = y `div` 8
  let row = y `mod` 8

  line <- concat <$> forM [0 .. tilesWide - 1] (\tx -> do
    tileRow <- getTileRow nametable (tx, ty) row
    pure $ map (tileRow V.!) [0..7]
    )

  pure $ V.fromList line

getTileRowPatterns :: Word16 -> (Int, Int) -> Int -> IOEmulator (Word8, Word8)
getTileRowPatterns nameTableAddr (x, y) row = do
  let index = (y * tilesWide) + x
  let addr = 0x2000 + 0x400 * nameTableAddr + (fromIntegral index)
  pattern <- load (Ppu $ PpuMemory8 addr)
  backgroundPatternAddr <- load $ (Ppu BackgroundTableAddr)

  let basePatternAddr = (toInt pattern) * 16 + row
  let patternTableAddr = backgroundPatternAddr + fromIntegral basePatternAddr

  pattern1 <- load (Ppu $ PpuMemory8 patternTableAddr)
  pattern2 <- load (Ppu $ PpuMemory8 $ patternTableAddr + 8)

  pure (pattern1, pattern2)

getTileAttribute :: Word16 -> (Int, Int) -> IOEmulator Word8
getTileAttribute nameTableAddr (x, y) = do
  let gx = x `div` 4
  let gy = y `div` 4
  let sx = (x `mod` 4) `div` 2
  let sy = (y `mod` 4) `div` 2
  let addr = fromIntegral $ 0x23c0 + (0x400 * (fromIntegral nameTableAddr)) + (gy * 8) + gx
  attribute <- load (Ppu $ PpuMemory8 addr)
  let shift = fromIntegral $ ((sy * 2) + sx) * 2
  pure $ (attribute `shiftR` shift) .&. 3

getTileRow :: Word16 -> (Int, Int) -> Int -> IOEmulator (V.Vector Word8)
getTileRow nameTableAddr coords row = do
  (pattern1, pattern2) <- getTileRowPatterns nameTableAddr coords row
  attribute <- getTileAttribute nameTableAddr coords
  let row = [(pattern1 `shiftR` x, pattern2 `shiftR` x) | x <- reverse [0..7]]
  let row' = [ (x .&. 1, (y .&. 1) `shiftL` 1)  | (x, y) <- row]
  let indexes = [toInt $ (attribute `shiftL` 2) .|. x .|. y | (x, y) <- row']
  items <- sequence $ [load $ Ppu $ PaletteData i | i <- indexes]
  pure $ V.fromList items

tilesWide :: Int
tilesWide = 32

palette :: V.Vector (Word8, Word8, Word8)
palette = V.fromList
  [ (0x66, 0x66, 0x66), (0x00, 0x2A, 0x88),
    (0x14, 0x12, 0xA7), (0x3B, 0x00, 0xA4),
    (0x5C, 0x00, 0x7E), (0x6E, 0x00, 0x40),
    (0x6C, 0x06, 0x00), (0x56, 0x1D, 0x00),
    (0x33, 0x35, 0x00), (0x0B, 0x48, 0x00),
    (0x00, 0x52, 0x00), (0x00, 0x4F, 0x08),
    (0x00, 0x40, 0x4D), (0x00, 0x00, 0x00),
    (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xAD, 0xAD, 0xAD), (0x15, 0x5F, 0xD9),
    (0x42, 0x40, 0xFF), (0x75, 0x27, 0xFE),
    (0xA0, 0x1A, 0xCC), (0xB7, 0x1E, 0x7B),
    (0xB5, 0x31, 0x20), (0x99, 0x4E, 0x00),
    (0x6B, 0x6D, 0x00), (0x38, 0x87, 0x00),
    (0x0C, 0x93, 0x00), (0x00, 0x8F, 0x32),
    (0x00, 0x7C, 0x8D), (0x00, 0x00, 0x00),
    (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xFF, 0xFE, 0xFF), (0x64, 0xB0, 0xFF),
    (0x92, 0x90, 0xFF), (0xC6, 0x76, 0xFF),
    (0xF3, 0x6A, 0xFF), (0xFE, 0x6E, 0xCC),
    (0xFE, 0x81, 0x70), (0xEA, 0x9E, 0x22),
    (0xBC, 0xBE, 0x00), (0x88, 0xD8, 0x00),
    (0x5C, 0xE4, 0x30), (0x45, 0xE0, 0x82),
    (0x48, 0xCD, 0xDE), (0x4F, 0x4F, 0x4F),
    (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xFF, 0xFE, 0xFF), (0xC0, 0xDF, 0xFF),
    (0xD3, 0xD2, 0xFF), (0xE8, 0xC8, 0xFF),
    (0xFB, 0xC2, 0xFF), (0xFE, 0xC4, 0xEA),
    (0xFE, 0xCC, 0xC5), (0xF7, 0xD8, 0xA5),
    (0xE4, 0xE5, 0x94), (0xCF, 0xEF, 0x96),
    (0xBD, 0xF4, 0xAB), (0xB3, 0xF3, 0xCC),
    (0xB5, 0xEB, 0xF2), (0xB8, 0xB8, 0xB8),
    (0x00, 0x00, 0x00), (0x00, 0x00, 0x00) ]


fetchNameTableValue :: IOEmulator Word8
fetchNameTableValue = do
  v <- load $ Ppu CurrentVRamAddr
  let addr = PpuMemory8 (0x2000 .|. (v .&. 0x0FFF))
  load $ Ppu addr

fetchAttributeTableValue :: IOEmulator Word8
fetchAttributeTableValue = do
  v <- load $ Ppu CurrentVRamAddr
  let addr = PpuMemory8 $ 0x23C0 .|. (v .&. 0x0C00) .|. ((v `shiftR` 4) .&. 0x38) .|. ((v `shiftR` 2) .&. 0x07)
  v' <- load $ Ppu addr
  let shift = fromIntegral $ ((v `shiftR` 4) .&. 4) .|. (v .&. 2)
  pure $ ((v' `shiftR` shift) .&. 3) `shiftL` 2

fetchLowTileValue :: Word8 -> IOEmulator Word8
fetchLowTileValue nametable = do
  v <- load $ Ppu CurrentVRamAddr
  bt <- load $ Ppu BackgroundTableAddr
  let fineY = (v `shiftR` 12) .&. 7
  let addr = PpuMemory8 $ (0x1000 * bt) + (fromIntegral nametable) * 16 + fineY
  load $ Ppu addr

fetchHighTileValue :: Word8 -> IOEmulator Word8
fetchHighTileValue nametable = do
  v <- load $ Ppu CurrentVRamAddr
  bt <- load $ Ppu BackgroundTableAddr
  let fineY = (v `shiftR` 12) .&. 7
  let addr = PpuMemory8 $ (0x1000 * bt) + (fromIntegral nametable) * 16 + fineY + 8
  load $ Ppu addr


