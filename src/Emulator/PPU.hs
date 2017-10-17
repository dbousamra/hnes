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

data RenderPhase
  = PreRender
  | Visible
  | PostRender
  | VBlank
  deriving (Eq, Show)

renderPhase :: Int -> RenderPhase
renderPhase scanline
  | scanline >= 0 && scanline <= 239 = Visible
  | scanline == 240 = PostRender
  | scanline >= 241 && scanline <= 260 = VBlank
  | scanline == 261 = PreRender
  | otherwise = error $ "Erroneous render phase detected at scanline " ++ show scanline


reset :: IOEmulator ()
reset = do
  store (Ppu PpuCycles) 340
  store (Ppu Scanline) 240
  store (Ppu VerticalBlank) False

step :: IOEmulator ()
step = do
  scanline <- load (Ppu Scanline)
  cycles <- load (Ppu PpuCycles)

  renderScanline scanline cycles

  -- Update the counters, cycles etc
  tick

  pure ()

tick :: IOEmulator ()
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


renderScanline :: Int -> Int -> IOEmulator ()
renderScanline scanline cycle = do
  let phase = renderPhase scanline

  case (phase, cycle) of
    (VBlank, 1) ->
      enterVBlank

    (PostRender, _) ->
      pure ()

    (PreRender, c) -> do

      renderSprites cycle
      renderBackground scanline cycle

      when (c == 1) $ do
        -- status.sprOvf = status.sprHit = false;
        exitVBlank

      when (c >= 280 && c <= 304) $
        vUpdate

      when (c == 340 && True && True) $
        modify (Ppu PpuCycles) (+1)

      pure ()

    (Visible, c) -> do
      renderSprites cycle
      renderBackground scanline cycle

    other ->
      pure ()


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
  when (scanline < 240 && x >= 0 && x <= 256) $ do
    let addr = Ppu $ Screen (x, scanline)
    store addr (255, 0, 0)

