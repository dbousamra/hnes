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

data VisiblePhase
 = Idle
 | VisibleCycle
 | PreFetchCycle
 deriving (Eq, Show)

data RenderPhase
  = PreRender
  | VisibleLine VisiblePhase
  | PostRender
  | VBlank
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
    VBlank                    -> enterVBlank
    PreRender                 -> exitVBlank
    PostRender                -> idle
    VisibleLine Idle          -> idle
    VisibleLine PreFetchCycle -> fetchPhase scanline cycle
    VisibleLine VisibleCycle  -> renderPixels scanline cycle >> fetchPhase scanline cycle

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
getRenderPhase scanline cycle =
  if (scanline == 240) then
    PostRender
  else if (scanline == 261) then
    PreRender
  else if (scanline >= 241 && scanline <= 260) then
    VBlank
  else if (scanline >= 0 && scanline <= 239) then
    if (cycle == 0) then
      VisibleLine Idle
    else if (cycle >= 1 && cycle <= 256) then
      VisibleLine VisibleCycle
    else
      VisibleLine PreFetchCycle
  else
    error $ "Erroneous render phase detected at scanline " ++ show scanline

idle :: IOEmulator ()
idle = pure ()

fetchPhase :: Int -> Int -> IOEmulator ()
fetchPhase scanline cycle = do
  pure ()

enterVBlank :: IOEmulator ()
enterVBlank = do
  store (Ppu VerticalBlank) True
  generateNMI <- load (Ppu GenerateNMI)
  when generateNMI $ store (Cpu Interrupt) (Just NMI)

exitVBlank :: IOEmulator ()
exitVBlank = store (Ppu VerticalBlank) False

renderPixels :: Int -> Int -> IOEmulator ()
renderPixels scanline cycle = do
  let x = cycle - 1
  let y = scanline

  atv <- fetchAttributeTableValue
  ntv <- fetchNameTableValue
  lotv <- fetchLowTileValue ntv
  hitv <- fetchHighTileValue ntv

  let tileData = do
        i <- [0..7]
        let p1 = ((lotv `shiftL` i) .&. 0x80) `shiftR` 7
        let p2 = ((hitv `shiftL` i) .&. 0x80) `shiftR` 6
        pure $ atv .|. p1 .|. p2

  let tileData' = foldl op 0 tileData
        where op acc i = (acc `shiftL` 4) .|. i

  let backgroundPixel = (tileData' `shiftR` 32) .&. 0x0F

  liftIO $ putStrLn (show backgroundPixel)

  store (Ppu $ Screen (x, y)) (backgroundPixel, 0, 0)

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


