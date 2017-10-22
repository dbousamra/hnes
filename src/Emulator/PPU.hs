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

data PreRenderPhase
  = PreRenderReloadY
  | PreRenderClearVBlank
  | PreRenderIdle
  deriving (Eq, Show)

data RenderPhase
  = RenderVisible
  | RenderPreFetch
  | RenderIdle
  deriving (Eq, Show)

data FramePhase
  = PreRender PreRenderPhase
  | Render RenderPhase
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

  let framePhase = getFramePhase scanline cycle

  case framePhase of
    PreRender preRenderPhase -> handlePreRenderPhase preRenderPhase
    Render RenderIdle        -> idle
    Render RenderPreFetch    -> fetchData scanline cycle
    Render RenderVisible     -> renderPixel scanline cycle
    PostRender               -> idle
    VBlank                   -> enterVBlank

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

getRenderPhase :: Int -> RenderPhase
getRenderPhase cycle
  | cycle == 0 = RenderIdle
  | cycle >= 1 && cycle <= 256 = RenderVisible
  | cycle >= 321 && cycle <= 336 = RenderPreFetch
  | otherwise = RenderIdle

getPreRenderPhase :: Int -> PreRenderPhase
getPreRenderPhase cycle
  | cycle == 1 = PreRenderClearVBlank
  | cycle >= 280 && cycle <= 304 = PreRenderReloadY
  | otherwise = PreRenderIdle

getFramePhase :: Int -> Int -> FramePhase
getFramePhase scanline cycle
  | scanline >= 0 && scanline <= 239 = Render $ getRenderPhase cycle
  | scanline == 240 = PostRender
  | scanline >= 241 && scanline <= 260 = VBlank
  | scanline == 261 = PreRender $ getPreRenderPhase cycle
  | otherwise = error $ "Erronenous frame phase detected at scanline "
    ++ show scanline ++ " and cycle "
    ++ show cycle

handlePreRenderPhase :: PreRenderPhase -> IOEmulator ()
handlePreRenderPhase preRenderPhase = idle

idle :: IOEmulator ()
idle = pure ()

enterVBlank :: IOEmulator ()
enterVBlank = do
  store (Ppu VerticalBlank) True
  generateNMI <- load (Ppu GenerateNMI)
  when generateNMI $ store (Cpu Interrupt) (Just NMI)

exitVBlank :: IOEmulator ()
exitVBlank = store (Ppu VerticalBlank) False

renderPixel :: Int -> Int -> IOEmulator ()
renderPixel scanline cycle = pure ()

fetchData :: Int -> Int -> IOEmulator ()
fetchData scanline cycle = case cycle `mod` 8 of
  1     -> fetchNameTableValue >>= store (Ppu NameTableByte)
  3     -> fetchAttrTableValue >>= store (Ppu AttrTableByte)
  5     -> fetchLoTileValue    >>= store (Ppu LoTileByte)
  7     -> fetchHiTileValue    >>= store (Ppu HiTileByte)
  0     -> storeTileData
  other -> idle


fetchNameTableValue :: IOEmulator Word8
fetchNameTableValue = do
  v <- load $ Ppu CurrentVRamAddr
  let addr = PpuMemory8 (0x2000 .|. (v .&. 0x0FFF))
  load $ Ppu addr

fetchAttrTableValue :: IOEmulator Word8
fetchAttrTableValue = do
  v <- load $ Ppu CurrentVRamAddr
  let addr = PpuMemory8 $ 0x23C0 .|. (v .&. 0x0C00) .|. ((v `shiftR` 4) .&. 0x38) .|. ((v `shiftR` 2) .&. 0x07)
  v' <- load $ Ppu addr
  let shift = fromIntegral $ ((v `shiftR` 4) .&. 4) .|. (v .&. 2)
  pure $ ((v' `shiftR` shift) .&. 3) `shiftL` 2

fetchLoTileValue :: IOEmulator Word8
fetchLoTileValue = do
  v <- load $ Ppu CurrentVRamAddr
  bt <- load $ Ppu BackgroundTableAddr
  nametableV <- load $ Ppu NameTableByte
  let fineY = (v `shiftR` 12) .&. 7
  let addr = PpuMemory8 $ (0x1000 * bt) + (fromIntegral nametableV) * 16 + fineY
  load $ Ppu addr

fetchHiTileValue :: IOEmulator Word8
fetchHiTileValue = do
  v <- load $ Ppu CurrentVRamAddr
  bt <- load $ Ppu BackgroundTableAddr
  nametableV <- load $ Ppu NameTableByte
  let fineY = (v `shiftR` 12) .&. 7
  let addr = PpuMemory8 $ (0x1000 * bt) + (fromIntegral nametableV) * 16 + fineY + 8
  load $ Ppu addr

storeTileData :: IOEmulator ()
storeTileData = idle
