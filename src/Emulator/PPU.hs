module Emulator.PPU (
    reset
  , step
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits                   (shiftL, shiftR, (.&.), (.|.))
import           Data.List                   (intercalate)
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Monad
import           Emulator.Nes
import           Emulator.Util
import           System.Random

reset :: IOEmulator ()
reset = do
  store (Ppu PpuCycles) 340
  store (Ppu Scanline) 240
  store (Ppu VerticalBlank) False

renderScanline :: IOEmulator ()
renderScanline = do
  nametable <- load (Ppu NameTableAddr)
  y <- load (Ppu Scanline)
  let ty = y `div` 8
  let row = y `mod` 8

  forM_ [0 .. tilesWide - 1] (\tx -> do
    tileRow <- getTileRow nametable (tx, ty) row
    forM_ [0 .. 7] (\i -> do
      -- offset within pixel, from start of tile
      let x = tx * 8 + i
      let tile = tileRow !! i
      let color = palette !! fromIntegral tile
      let addr = Ppu $ Screen (x, y)
      store addr color
      ))

step :: IOEmulator ()
step = do
  -- Update the counters, cycles etc
  tick

  scanline <- load (Ppu Scanline)
  cycles <- load (Ppu PpuCycles)

  -- Draw scanlines
  when (scanline < 240 && cycles == 1) $
    renderScanline

  -- Enter Vertical blank period
  when ((scanline == 241 && cycles == 1)) $
    store (Ppu VerticalBlank) True

  -- Exit Vertical blank period
  when ((scanline == 261 && cycles == 1)) $
    store (Ppu VerticalBlank) False

tick :: IOEmulator ()
tick = do
  modify (Ppu PpuCycles) (+1)
  cycles <- load $ Ppu PpuCycles

  when (cycles > 340) $ do
    modify (Ppu PpuCycles) (const 0)
    modify (Ppu Scanline) (+1)
    scanline <- load (Ppu Scanline)

    when (scanline > 261) $ do
      modify (Ppu Scanline) (const 0)
      modify (Ppu FrameCount) (+1)

modify :: Address a -> (a -> a) -> IOEmulator ()
modify addr f = do
  av <- load addr
  store addr (f av)

getTileRowPatterns :: Word16 -> (Int, Int) -> Int -> IOEmulator (Word8, Word8)
getTileRowPatterns nameTableAddr (x, y) row = do
  let index = (y * tilesWide) + x
  let addr = 0x2000 + 0x400 * nameTableAddr + (fromIntegral index)
  pattern <- load (Ppu $ PpuMemory8 addr)
  patternTableAddr <- load $ (Ppu BackgroundTableAddr)
  let patternAddr1 = case patternTableAddr of
        BackgroundTable0000 -> toWord16 $ pattern * 16
        BackgroundTable1000 -> toWord16 $ 0x1000 + pattern * 16
  let patternAddr2 = patternAddr1 + 8
  pattern1 <- load (Ppu $ PpuMemory8 patternAddr1)
  pattern2 <- load (Ppu $ PpuMemory8 patternAddr2)
  pure (pattern1, pattern2)

getTileRow :: Word16 -> (Int, Int) -> Int -> IOEmulator [Word8]
getTileRow nameTableAddr coords row = do
  (pattern1, pattern2) <- getTileRowPatterns nameTableAddr coords row
  let row = [(pattern1 `shiftR` x, pattern2 `shiftR` x) | x <- [0..7]]
  let row' = [ (x .&. 1, (y .&. 1) `shiftL` 1)  | (x, y) <- row]
  let indexes = [toInt $ x .|. y | (x, y) <- row']
  sequence $ [load $ Ppu $ PaletteData i | i <- indexes]

tilesWide :: Int
tilesWide = 32

palette :: [(Word8, Word8, Word8)]
palette =
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
