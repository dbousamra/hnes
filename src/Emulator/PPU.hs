module Emulator.PPU (
    reset
  , step
) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Monad
import           Emulator.Nes
import           Emulator.Util
import           System.Random

reset :: MonadEmulator m => m ()
reset = pure ()

renderScanline :: (MonadIO m, MonadEmulator m) => m ()
renderScanline = do
  y <- load (Ppu Scanline)
  nametable <- load (Ppu NameTableAddr)
  let ty = y `div` 8
  let row = y `mod` 8

  forM_ [0 .. tilesWide - 1] (\tx -> do
    -- tileRow <- getTileRow nametable (tx, ty) row
    r <- liftIO $ randomRIO (1, 255)
    g <- liftIO $ randomRIO (1, 255)
    b <- liftIO $ randomRIO (1, 255)
    forM_ [0 .. 7] (\i -> do
      -- offset within pixel, from start of tile
      let x = tx * 8 + i
      -- let tile = tileRow !! i
      -- let color = getColor tile
      let color = (r, g, b)
      let addr = Ppu $ Screen (x, y)
      store addr color
      ))


  pure ()
  -- forM_ [0 .. 256 - 1] (\x -> do
  --   -- r <- liftIO $ randomRIO (1, 255)
  --   -- g <- liftIO $ randomRIO (1, 255)
  --   -- b <- liftIO $ randomRIO (1, 255)
  --   let addr = Ppu $ (Screen (x, y))
  --   store addr (100, 20, 180))

step :: (MonadIO m, MonadEmulator m) => m ()
step = do
  -- Update the counters, cycles etc
  tick

  scanline <- load (Ppu Scanline)
  cycles <- load (Ppu PpuCycles)

  -- Draw scanlines
  when (scanline < 240 && cycles == 1) $
    renderScanline

  -- Enter VBlank period
  when ((scanline == 241 && cycles == 1)) $
    store (Ppu VBlank) True

  -- Exit VBlank period
  when ((scanline == 261 && cycles == 1)) $
    store (Ppu VBlank) False

tick :: MonadEmulator m => m ()
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

modify :: MonadEmulator m => Address a -> (a -> a) -> m ()
modify addr f = do
  av <- load addr
  store addr (f av)

getTileRowPatterns :: (MonadIO m, MonadEmulator m) => Word16 -> (Int, Int) -> Int -> m (Word8, Word8)
getTileRowPatterns nameTableAddr (x, y) row = do
  let index = (y * tilesWide) + x
  let addr = 0x2000 + 0x400 * nameTableAddr + (fromIntegral index)
  pattern <- load $ (Ppu $ PpuMemory8 addr)
  pure (1, 1)

getTileRow :: (MonadIO m, MonadEmulator m) => Word16 -> (Int, Int) -> Int -> m [Word8]
getTileRow nameTableAddr coords row = do
  patterns <- getTileRowPatterns nameTableAddr coords row
  liftIO $ putStrLn $ show patterns
  pure []

getColor :: Word8 -> (Word8, Word8, Word8)
getColor tile = (255, 10, 20)

tilesWide :: Int
tilesWide = 32
