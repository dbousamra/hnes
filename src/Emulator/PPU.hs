module Emulator.PPU (
    reset
  , step
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits              (shiftL, shiftR, xor, (.&.), (.|.))
import           Data.Char              (toLower)
import           Data.IORef
import           Data.Maybe             (fromMaybe)
import qualified Data.Vector            as V
import           Data.Word
import           Emulator.Monad
import           Emulator.Nes
import           Emulator.Util
import           Prelude                hiding (cycle)

reset :: IOEmulator ()
reset = do
  store (Ppu PpuCycles) 340
  store (Ppu Scanline) 240
  store (Ppu VerticalBlank) False

step :: IOEmulator ()
step = do
  (scanline, cycle) <- tick
  handleLinePhase scanline cycle

tick :: IOEmulator Coords
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

  scanline' <- load $ Ppu Scanline
  cycles' <- load $ Ppu PpuCycles
  pure (scanline', cycles')

-- tick :: IOEmulator Coords
-- tick = do
--   cycles <- load $ Ppu PpuCycles
--   scanline <- load $ Ppu Scanline

--   if (scanline == 261 && cycles == 339) then do
--     store (Ppu PpuCycles) 0
--     store (Ppu Scanline) 0
--     modify (Ppu FrameCount) (+1)
--   else do
--     modify (Ppu PpuCycles) (+1)

--     when (cycles > 340) $ do
--       store (Ppu PpuCycles) 0
--       modify (Ppu Scanline) (+1)
--       scanline <- load (Ppu Scanline)

--       when (scanline > 261) $ do
--         store (Ppu Scanline) 0
--         modify (Ppu FrameCount) (+1)

--   scanline' <- load $ Ppu Scanline
--   cycles' <- load $ Ppu PpuCycles
--   pure (scanline', cycles')

handleLinePhase :: Int -> Int -> IOEmulator ()
handleLinePhase scanline cycle = do
  let preLine = scanline == 261
  let visibleLine = scanline < 240
  let renderLine = preLine || visibleLine

  let visibleCycle = cycle >= 1 && cycle <= 256
  let preFetchCycle = cycle >= 321 && cycle <= 336
  let fetchCycle = visibleCycle || (cycle >= 321 && cycle <= 336)

  bgVisible <- load (Ppu BackgroundVisible)
  spritesVisible <- load (Ppu SpritesVisible)

  let rendering = bgVisible || spritesVisible

  when rendering $ do
    when (visibleLine && visibleCycle) $
      renderPixel scanline cycle

    when (renderLine && fetchCycle) $
      fetch scanline cycle

    when (preLine && cycle >= 280 && cycle <= 304) $
      copyY

    when (preLine || visibleLine) $ do
      when ((preFetchCycle || visibleCycle) && cycle `mod` 8 == 0)
        incrementX

      when (cycle == 256)
        incrementY

      when (cycle == 257)
        copyX

    when (cycle == 257) $
      if visibleLine then
        evaluateSprites scanline
      else
        pure ()

  when (scanline == 241 && cycle == 1) $
    enterVBlank

  when (preLine && cycle == 1) $
    exitVBlank

renderPixel :: Int -> Int -> IOEmulator ()
renderPixel scanline cycle = do
  let coords = (cycle - 1, scanline)
  bgColor <- getBackgroundPixel coords
  spriteColor <- getSpritePixel coords
  finalColor <- getComposedColor bgColor spriteColor
  store (Ppu $ Screen coords) finalColor

getBackgroundPixel :: Coords -> IOEmulator Word8
getBackgroundPixel coords = do
  tileData <- fetchTileData
  pure $ fromIntegral $ (tileData `shiftR` (7 * 4)) .&. 0x0F -- TODO: FIX?

getSpritePixel :: Coords -> IOEmulator Word8
getSpritePixel coords = do
  sprites <- load (Ppu Sprites)
  let colors = V.map getColor sprites
  pure $ fromMaybe 0 (msum colors)
  where
    getColor :: Sprite -> Maybe Word8
    getColor (Sprite _ (x, y) _ _ sPattern) = do
      let offset = fst coords - x
      if offset >= 0 && offset <= 7 then do
        let nextOffset = fromIntegral ((7 - offset) * 4) :: Word8
        let shifted = fromIntegral (sPattern `shiftR` fromIntegral nextOffset) :: Word8
        let color = shifted .&. 0x0F
        if color `mod` 4 /= 0 then Just color else Nothing
      else
        Nothing


getComposedColor :: Word8 -> Word8 -> IOEmulator Color
getComposedColor bg sprite = do
  index <- load $ Ppu $ PpuMemory8 (0x3F00 + fromIntegral color)
  pure $ getPaletteColor (index `mod` 64)
  where
    b = bg `mod` 4 /= 0
    s = sprite `mod` 4 /= 0
    color =  sprite .|. 0x10
    -- color =
    --   if not b && not s then
    --     0
    --   else if not b && s then
    --     sprite .|. 0x10
    --   else if b && not s then
    --     bg
    --   else
    --     sprite .|. 0x10

readPalette :: Word16 -> IOEmulator Word8
readPalette addr = load $ Ppu $ PaletteData addr'
  where addr' = if (addr >= 16) && (addr `mod` 4 == 0) then addr - 16 else addr

fetch :: Int -> Int -> IOEmulator ()
fetch scanline cycle = do
  modify (Ppu TileData) (`shiftL` 4)
  case cycle `mod` 8 of
    1 -> fetchNameTableValue
    3 -> fetchAttributeTableValue
    5 -> fetchLowTileValue
    7 -> fetchHighTileValue
    0 -> storeTileData
    _ -> idle

fetchNameTableValue :: IOEmulator ()
fetchNameTableValue = do
  v <- load $ Ppu CurrentVRamAddr
  let addr = PpuMemory8 (0x2000 .|. (v .&. 0x0FFF))
  ntv <- load $ Ppu addr
  store (Ppu NameTableByte) ntv

fetchAttributeTableValue :: IOEmulator ()
fetchAttributeTableValue = do
  v <- load $ Ppu CurrentVRamAddr
  let addr = PpuMemory8 $ 0x23C0 .|. (v .&. 0x0C00) .|. ((v `shiftR` 4) .&. 0x38) .|. ((v `shiftR` 2) .&. 0x07)
  v' <- load $ Ppu addr
  let shift = fromIntegral $ ((v `shiftR` 4) .&. 4) .|. (v .&. 2)
  let atv = ((v' `shiftR` shift) .&. 3) `shiftL` 2
  store (Ppu AttrTableByte) atv

fetchLowTileValue :: IOEmulator ()
fetchLowTileValue = do
  v <- load $ Ppu CurrentVRamAddr
  let fineY = (v `shiftR` 12) .&. 7
  bt <- load $ Ppu BackgroundTableAddr
  ntv <- load $ Ppu NameTableByte
  let addr = PpuMemory8 $ bt + (fromIntegral ntv) * 16 + fineY
  ltv <- load $ Ppu addr
  store (Ppu LoTileByte) ltv

fetchHighTileValue :: IOEmulator ()
fetchHighTileValue = do
  ntv <- load $ Ppu NameTableByte
  v <- load $ Ppu CurrentVRamAddr
  bt <- load $ Ppu BackgroundTableAddr
  let fineY = (v `shiftR` 12) .&. 7
  let addr = PpuMemory8 $ bt + (fromIntegral ntv) * 16 + fineY + 8
  htv <- load $ Ppu addr
  store (Ppu HiTileByte) htv

fetchTileData :: IOEmulator Word32
fetchTileData = do
  tileData <- load $ Ppu TileData
  pure $ fromIntegral $ tileData `shiftR` 32

storeTileData :: IOEmulator ()
storeTileData = do
  lotv <- load $ Ppu LoTileByte
  hitv <- load $ Ppu HiTileByte
  atv <- load $ Ppu AttrTableByte

  let tileData = do
        i <- [0..7]
        let p1 = ((lotv `shiftL` i) .&. 0x80) `shiftR` 7
        let p2 = ((hitv `shiftL` i) .&. 0x80) `shiftR` 6
        pure $ fromIntegral $ atv .|. p1 .|. p2 :: [Word32]

  let tileData' = foldl op 0 tileData
       where op acc i = (acc `shiftL` 4) .|. i

  modify (Ppu TileData) (\x -> x .|. (fromIntegral tileData'))

copyY :: IOEmulator ()
copyY = do
  tv <- load (Ppu TempVRamAddr)
  cv <- load (Ppu CurrentVRamAddr)
  store (Ppu CurrentVRamAddr) ((cv .&. 0x841F) .|. (tv .&. 0x7BE0))

copyX :: IOEmulator ()
copyX = do
  tv <- load (Ppu TempVRamAddr)
  cv <- load (Ppu CurrentVRamAddr)
  store (Ppu CurrentVRamAddr) ((cv .&. 0xFBE0) .|. (tv .&. 0x041F))

incrementX :: IOEmulator ()
incrementX = do
  v <- load $ Ppu CurrentVRamAddr
  if v .&. 0x001F == 31 then
    modify (Ppu CurrentVRamAddr) ((.&. 0xFFE0) . (`xor` 0x0400))
  else
    modify (Ppu CurrentVRamAddr) (+ 1)

incrementY :: IOEmulator ()
incrementY = do
  v <- load $ Ppu CurrentVRamAddr
  if v .&. 0x7000 /= 0x7000 then
    modify (Ppu CurrentVRamAddr) (+ 0x1000)
  else do
    modify (Ppu CurrentVRamAddr) (.&. 0x8FFF)
    let y = (v .&. 0x03E0) `shiftR` 5

    y' <- if y == 29 then do
      modify (Ppu CurrentVRamAddr) (`xor` 0x0800)
      pure 0
    else if y == 31 then
      pure 0
    else
      pure $ y + 1

    v' <- load $ Ppu CurrentVRamAddr
    store (Ppu CurrentVRamAddr) ((v' .&. 0xFC1F) .|. (y' `shiftL` 5))

getSpriteAt :: Int -> SpriteSize -> Int -> IOEmulator Sprite
getSpriteAt scanline size i = do
  let baseOffset = fromIntegral $ i * 4
  y <- load (Ppu $ OamData $ baseOffset + 0)
  tileIndexByte <- load (Ppu $ OamData $ baseOffset + 1)
  attributeByte <- load (Ppu $ OamData $ baseOffset + 2)
  x <- load (Ppu $ OamData $ baseOffset + 3)
  let row =  scanline - fromIntegral y
  addr <- getSpriteAddress row size attributeByte tileIndexByte
  loTileByte <- load (Ppu $ PpuMemory8 addr)
  hiTileByte <- load (Ppu $ PpuMemory8 $ addr + 8)
  let spritePattern = decodeSpritePattern attributeByte loTileByte hiTileByte
  pure $ Sprite i (fromIntegral x, fromIntegral $ y) tileIndexByte attributeByte spritePattern

decodeSpritePattern :: Word8 -> Word8 -> Word8 -> Word32
decodeSpritePattern attr lo hi = tileData'
  where
  atv = (attr .&. 3) `shiftL` 2
  tileData = do
    i <- [0..7]
    let p1 = ((lo `shiftL` i) .&. 0x80) `shiftR` 7
    let p2 = ((hi `shiftL` i) .&. 0x80) `shiftR` 6
    pure $ fromIntegral $ atv .|. p1 .|. p2 :: [Word32]
  tileData' = foldl op 0 tileData
       where op acc i = (acc `shiftL` 4) .|. i

getSpriteAddress :: Int -> SpriteSize -> Word8 -> Word8 -> IOEmulator Word16
getSpriteAddress row size attr tile = case size of
  Double -> do
    let row' = if attr .&. 0x80 == 0x80 then 7 - row else row
    table <- load (Ppu SpriteTableAddr)
    pure $ table + (fromIntegral tile) * 16 + (fromIntegral row')
  Normal -> do
    let row' = if attr .&. 0x80 == 0x80 then 15 - row else row
    let table = tile .&. 1
    let tile' = tile .&. 0xFE
    let (tile'', row'') = if (row' > 7) then (tile' + 1, row' - 8) else (tile, row')
    pure $ (0x1000 * fromIntegral table) + (fromIntegral tile) * 16 + (fromIntegral row')

evaluateSprites :: Int -> IOEmulator ()
evaluateSprites scanline = do
  spriteSize <- load $ Ppu SpriteSize
  sprites <- traverse (getSpriteAt scanline spriteSize) (V.fromList [0..63])
  -- let visibleSprites = V.take 8 $ V.filter (isSpriteVisible scanline spriteSize) sprites
  let visibleSprites = V.take 3 $ sprites
  store (Ppu Sprites) visibleSprites

isSpriteVisible :: Int -> SpriteSize -> Sprite -> Bool
isSpriteVisible scanline spriteSize (Sprite i (x, y) _ _ _) = row < 0 || row >= h
  where
    row = scanline - y
    h = case spriteSize of
          Normal -> 8
          Double -> 16

enterVBlank :: IOEmulator ()
enterVBlank = do
  store (Ppu VerticalBlank) True
  generateNMI <- load (Ppu GenerateNMI)
  when generateNMI $ store (Cpu Interrupt) (Just NMI)

exitVBlank :: IOEmulator ()
exitVBlank = store (Ppu VerticalBlank) False

idle :: IOEmulator ()
idle = pure ()

getPaletteColor :: Word8 -> Color
getPaletteColor index = palette V.! fromIntegral index

palette :: V.Vector Color
palette = V.fromList
  [ (0x66, 0x66, 0x66), (0x00, 0x2A, 0x88), (0x14, 0x12, 0xA7), (0x3B, 0x00, 0xA4),
    (0x5C, 0x00, 0x7E), (0x6E, 0x00, 0x40), (0x6C, 0x06, 0x00), (0x56, 0x1D, 0x00),
    (0x33, 0x35, 0x00), (0x0B, 0x48, 0x00), (0x00, 0x52, 0x00), (0x00, 0x4F, 0x08),
    (0x00, 0x40, 0x4D), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xAD, 0xAD, 0xAD), (0x15, 0x5F, 0xD9), (0x42, 0x40, 0xFF), (0x75, 0x27, 0xFE),
    (0xA0, 0x1A, 0xCC), (0xB7, 0x1E, 0x7B), (0xB5, 0x31, 0x20), (0x99, 0x4E, 0x00),
    (0x6B, 0x6D, 0x00), (0x38, 0x87, 0x00), (0x0C, 0x93, 0x00), (0x00, 0x8F, 0x32),
    (0x00, 0x7C, 0x8D), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xFF, 0xFE, 0xFF), (0x64, 0xB0, 0xFF), (0x92, 0x90, 0xFF), (0xC6, 0x76, 0xFF),
    (0xF3, 0x6A, 0xFF), (0xFE, 0x6E, 0xCC), (0xFE, 0x81, 0x70), (0xEA, 0x9E, 0x22),
    (0xBC, 0xBE, 0x00), (0x88, 0xD8, 0x00), (0x5C, 0xE4, 0x30), (0x45, 0xE0, 0x82),
    (0x48, 0xCD, 0xDE), (0x4F, 0x4F, 0x4F), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xFF, 0xFE, 0xFF), (0xC0, 0xDF, 0xFF), (0xD3, 0xD2, 0xFF), (0xE8, 0xC8, 0xFF),
    (0xFB, 0xC2, 0xFF), (0xFE, 0xC4, 0xEA), (0xFE, 0xCC, 0xC5), (0xF7, 0xD8, 0xA5),
    (0xE4, 0xE5, 0x94), (0xCF, 0xEF, 0x96), (0xBD, 0xF4, 0xAB), (0xB3, 0xF3, 0xCC),
    (0xB5, 0xEB, 0xF2), (0xB8, 0xB8, 0xB8), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00) ]
