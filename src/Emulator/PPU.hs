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

renderPhase :: Int -> Int -> RenderPhase
renderPhase scanline cycle =
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

reset :: IOEmulator ()
reset = do
  store (Ppu PpuCycles) 340
  store (Ppu Scanline) 240
  store (Ppu VerticalBlank) False

step :: IOEmulator ()
step = do
  -- Update the counters, cycles etc
  (scanline, cycle) <- tick

  let renderPhase' = renderPhase scanline cycle

  when (renderPhase' == VBlank)
    enterVBlank

  when (renderPhase' == PreRender)
    exitVBlank






  -- renderScanline scanline cycles

  pure ()

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


-- renderScanline :: Int -> Int -> IOEmulator ()
-- renderScanline scanline cycle = do
--   let phase = renderPhase scanline

--   case (phase, cycle) of
--     (VBlank, 1) -> enterVBlank
--     (PostRender, _) -> pure ()
--     (PreRender, c) -> do
--       renderSprites cycle
--       renderBackground scanline cycle

--       when (c == 1) $ exitVBlank

--       when (c >= 280 && c <= 304) $ vUpdate

--       when (c == 340 && True && True) $ modify (Ppu PpuCycles) (+1)

--     (VisibleLine, c) -> do
--       -- renderSprites cycle
--       renderBackground scanline cycle

--     other -> pure ()


enterVBlank :: IOEmulator ()
enterVBlank = do
  store (Ppu VerticalBlank) True
  generateNMI <- load (Ppu GenerateNMI)
  when generateNMI $ store (Cpu Interrupt) (Just NMI)

exitVBlank :: IOEmulator ()
exitVBlank = store (Ppu VerticalBlank) False

hUpdate :: IOEmulator ()
hUpdate = pure ()

vUpdate :: IOEmulator ()
vUpdate = pure ()

vScroll :: IOEmulator ()
vScroll = pure ()

reloadShift :: IOEmulator ()
reloadShift = pure ()

renderSprites :: Int -> IOEmulator ()
renderSprites cycle = pure ()

renderBackground :: Int -> Int -> IOEmulator ()
renderBackground scanline cycle
  | cycle == 1 = do
    pure ()

  | cycle >= 2 && cycle <= 255 = do
    renderPixel scanline cycle

  | cycle == 256 = do
    renderPixel scanline cycle
    vScroll

  | cycle == 257 = do
    renderPixel scanline cycle
    reloadShift
    hUpdate

  | cycle == 321 = do
    pure ()

  | cycle >= 322 && cycle <= 337 = do
    renderPixel scanline cycle

  | cycle == 338 = do
    pure ()

  | cycle == 339 = do
    pure ()

  | cycle == 340 = do
    pure ()

  | otherwise = pure ()

renderPixel :: Int -> Int -> IOEmulator ()
renderPixel scanline cycle = do
  let x = cycle - 2
  let y = scanline
  when (y < 240 && x >= 0 && x <= 256) $ do
    store (Ppu $ Screen (x, y)) (255, 0, 0)

fetchNameTableValue :: IOEmulator Word8
fetchNameTableValue = do
  v <- load $ Ppu CurrentVRamAddr
  let addr = PpuMemory8 (0x2000 .|. (v .&. 0x0FFF))
  load $ Ppu addr

fetchAttributeTableValue :: IOEmulator Word8
fetchAttributeTableValue = do
  v <- load $ Ppu CurrentVRamAddr
  let addr = PpuMemory8 $ 0x23C0 .|. (v .&. 0x0C00) .|. ((v `shiftR` 4) .&. 0x38) .|. ((v `shiftR` 2) .&. 0x07)
  let shift = fromIntegral $ ((v `shiftR` 4) .&. 4) .|. (v .&. 2)
  v' <- load $ Ppu addr
  pure $ ((v' `shiftR` shift) .&. 3) `shiftL` 2

-- fetchLowTileValue :: IOEmulator Word8
-- fetchLowTileValue = do




