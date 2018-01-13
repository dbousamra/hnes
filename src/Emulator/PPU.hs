module Emulator.PPU (
    reset
  , step
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits              (unsafeShiftL, unsafeShiftR, xor, (.&.), (.|.))
import           Data.Char              (toLower)
import           Data.IORef
import           Data.Maybe             (fromMaybe)
import qualified Data.Vector            as V
import           Data.Word
import           Emulator.Nes
import           Emulator.Util
import           Prelude                hiding (cycle)

reset :: Emulator ()
reset = do
  storePpu ppuCycles 340
  storePpu scanline 240
  storePpu verticalBlank False

step :: Emulator ()
step = do
  (scanline, cycle) <- tick
  handleLinePhase scanline cycle

tick :: Emulator Coords
tick = do
  modifyPpu ppuCycles (+1)
  cycles <- loadPpu ppuCycles

  when (cycles > 340) $ do
    storePpu ppuCycles 0
    modifyPpu scanline (+1)
    scanline' <- loadPpu scanline

    when (scanline' > 261) $ do
      storePpu scanline 0
      modifyPpu frameCount (+1)

  scanline' <- loadPpu scanline
  cycles' <- loadPpu ppuCycles
  pure (scanline', cycles')

handleLinePhase :: Int -> Int -> Emulator ()
handleLinePhase scanline cycle = do
  let preLine = scanline == 261
  let visibleLine = scanline < 240
  let renderLine = preLine || visibleLine

  let visibleCycle = cycle >= 1 && cycle <= 256
  let preFetchCycle = cycle >= 321 && cycle <= 336
  let fetchCycle = visibleCycle || (cycle >= 321 && cycle <= 336)

  bgVisible <- loadPpu bgVisibility
  spritesVisible <- loadPpu spriteVisibility

  let rendering = bgVisible || spritesVisible

  when rendering $ do
    when (visibleLine && visibleCycle) $
      renderPixel scanline cycle

    when (renderLine && fetchCycle) $
      fetch scanline cycle

    when (preLine && cycle >= 280 && cycle <= 304)
      copyY

    when (preLine || visibleLine) $ do
      when ((preFetchCycle || visibleCycle) && cycle `rem` 8 == 0)
        incrementX

      when (cycle == 256)
        incrementY

      when (cycle == 257)
        copyX

    when (cycle == 257) $
      when visibleLine $
        evaluateSprites scanline

  when (scanline == 241 && cycle == 1)
    enterVBlank

  when (preLine && cycle == 1) $ do
    exitVBlank
    storePpu spriteZeroHit False

renderPixel :: Int -> Int -> Emulator ()
renderPixel scanline cycle = do
  let coords = (cycle - 1, scanline)
  bgColor <- getBackgroundPixel coords
  spriteColor <- getSpritePixel coords
  finalColor <- getComposedColor coords bgColor spriteColor
  writeScreen coords finalColor

getBackgroundPixel :: Coords -> Emulator Word8
getBackgroundPixel coords = do
  tileData <- fetchTileData
  fineX <- loadPpu fineX
  let scrolled = tileData `unsafeShiftR` fromIntegral ((7 - fineX) * 4)
  pure $ fromIntegral (scrolled .&. 0x0F)

getSpritePixel :: Coords -> Emulator (Maybe (Sprite, Word8))
getSpritePixel coords = do
  sprites' <- loadPpu sprites
  let colors = V.map getColor sprites'
  pure $ msum colors
  where
    getColor :: Sprite -> Maybe (Sprite, Word8)
    getColor sprite@(Sprite _ (x, y) _ _ sPattern _) = do
      let offset = fst coords - x
      if offset >= 0 && offset <= 7 then do
        let nextOffset = fromIntegral ((7 - offset) * 4) :: Word8
        let shifted = fromIntegral (sPattern `unsafeShiftR` fromIntegral nextOffset) :: Word8
        let color = shifted .&. 0x0F
        if color `rem` 4 /= 0
          then Just (sprite, color)
          else Nothing
      else
        Nothing

getComposedColor :: Coords -> Word8 -> Maybe (Sprite, Word8) -> Emulator Color
getComposedColor (x, y) bg sprite = do
  color <- getColor
  index <- readPpuMemory $ 0x3F00 + fromIntegral color
  pure $ getPaletteColor (index `rem` 64)
  where
    b = bg `rem` 4 /= 0
    (sc, ind, priority) = case sprite of
      Just (s, c) -> (c, sIndex s, sPriority s)
      Nothing     -> (0, 0, 0)
    s =  sc `rem` 4 /= 0
    getColor
      | not b && not s = pure 0
      | not b && s = pure $ sc .|. 0x10
      | b && not s = pure bg
      | otherwise = do
        storePpu spriteZeroHit (ind == 0 && x < 255)
        if priority == 0 then
          pure $ sc .|. 0x10
        else
          pure bg

fetch :: Int -> Int -> Emulator ()
fetch scanline cycle = do
  modifyPpu tileData (`unsafeShiftL` 4)
  case cycle `rem` 8 of
    1 -> fetchNameTableValue
    3 -> fetchAttributeTableValue
    5 -> fetchLowTileValue
    7 -> fetchHighTileValue
    0 -> storeTileData
    _ -> idle

fetchNameTableValue :: Emulator ()
fetchNameTableValue = do
  v <- loadPpu currentVramAddress
  ntv <- readPpuMemory $ 0x2000 .|. (v .&. 0x0FFF)
  storePpu nameTableByte ntv

fetchAttributeTableValue :: Emulator ()
fetchAttributeTableValue = do
  v <- loadPpu currentVramAddress
  v' <- readPpuMemory $ 0x23C0 .|. (v .&. 0x0C00) .|. ((v `unsafeShiftR` 4) .&. 0x38) .|. ((v `unsafeShiftR` 2) .&. 0x07)
  let shift = fromIntegral $ ((v `unsafeShiftR` 4) .&. 4) .|. (v .&. 2)
  let atv = ((v' `unsafeShiftR` shift) .&. 3) `unsafeShiftL` 2
  storePpu attrTableByte atv

fetchLowTileValue :: Emulator ()
fetchLowTileValue = do
  v <- loadPpu currentVramAddress
  let fineY = (v `unsafeShiftR` 12) .&. 7
  bt <- loadPpu bgTable
  ntv <- loadPpu nameTableByte
  ltv <- readPpuMemory $ bt + fromIntegral ntv * 16 + fineY
  storePpu loTileByte ltv

fetchHighTileValue :: Emulator ()
fetchHighTileValue = do
  ntv <- loadPpu nameTableByte
  v <- loadPpu currentVramAddress
  bt <- loadPpu bgTable
  let fineY = (v `unsafeShiftR` 12) .&. 7
  htv <- readPpuMemory $ bt + fromIntegral ntv * 16 + fineY + 8
  storePpu hiTileByte htv

fetchTileData :: Emulator Word32
fetchTileData = do
  td <- loadPpu tileData
  pure $ fromIntegral $ td `unsafeShiftR` 32

storeTileData :: Emulator ()
storeTileData = do
  lotv <- loadPpu loTileByte
  hitv <- loadPpu hiTileByte
  atv <- loadPpu attrTableByte

  let td = do
        i <- V.fromList [0..7]
        let p1 = ((lotv `unsafeShiftL` i) .&. 0x80) `unsafeShiftR` 7
        let p2 = ((hitv `unsafeShiftL` i) .&. 0x80) `unsafeShiftR` 6
        pure $ fromIntegral $ atv .|. p1 .|. p2 :: V.Vector Word32

  let td' = V.foldl' op 0 td
       where op acc i = (acc `unsafeShiftL` 4) .|. i

  modifyPpu tileData (\x -> x .|. fromIntegral td')


copyY :: Emulator ()
copyY = do
  tv <- loadPpu tempVramAddress
  cv <- loadPpu currentVramAddress
  storePpu currentVramAddress ((cv .&. 0x841F) .|. (tv .&. 0x7BE0))

copyX :: Emulator ()
copyX = do
  tv <- loadPpu tempVramAddress
  cv <- loadPpu currentVramAddress
  storePpu currentVramAddress ((cv .&. 0xFBE0) .|. (tv .&. 0x041F))

incrementX :: Emulator ()
incrementX = do
  v <- loadPpu currentVramAddress
  if v .&. 0x001F == 31 then do
    modifyPpu currentVramAddress (.&. 0xFFE0)
    modifyPpu currentVramAddress (`xor` 0x0400)
  else
    modifyPpu currentVramAddress (+ 1)

incrementY :: Emulator ()
incrementY = do
  v <- loadPpu currentVramAddress
  if v .&. 0x7000 /= 0x7000 then
    modifyPpu currentVramAddress (+ 0x1000)
  else do
    modifyPpu currentVramAddress (.&. 0x8FFF)
    let y = (v .&. 0x03E0) `unsafeShiftR` 5

    y' <- if y == 29 then do
      modifyPpu currentVramAddress (`xor` 0x0800)
      pure 0
    else if y == 31 then
      pure 0
    else
      pure $ y + 1

    v' <- loadPpu currentVramAddress
    storePpu currentVramAddress ((v' .&. 0xFC1F) .|. (y' `unsafeShiftL` 5))

evaluateSprites :: Int -> Emulator ()
evaluateSprites scanline = do
  spriteSize <- loadPpu spriteSize
  allSprites <- traverse (getSpriteAt scanline spriteSize) (V.fromList [0..63])
  let visibleSprites = V.take 8 (catMaybesV allSprites)
  storePpu sprites visibleSprites

getSpriteAt :: Int -> SpriteSize -> Int -> Emulator (Maybe Sprite)
getSpriteAt scanline size i = do
  let baseOffset = fromIntegral $ i * 4
  y <- readOAMData $ baseOffset + 0
  let row =  scanline - fromIntegral y

  if isSpriteVisible row size then do
    tileIndexByte <- readOAMData $ baseOffset + 1
    attrByte <- readOAMData $ baseOffset + 2
    x <- readOAMData $ baseOffset + 3
    addr <- getSpriteAddress row size attrByte tileIndexByte
    loTileByte <- readPpuMemory addr
    hiTileByte <- readPpuMemory $ addr + 8
    let spritePattern = decodeSpritePattern attrByte loTileByte hiTileByte
    let priority = (attrByte `unsafeShiftR` 5) .&. 1
    pure $ Just $ Sprite i (fromIntegral x, fromIntegral y) tileIndexByte attrByte spritePattern priority
  else
    pure Nothing

decodeSpritePattern :: Word8 -> Word8 -> Word8 -> Word32
decodeSpritePattern attr lo hi = tileData'
  where
  atv = (attr .&. 3) `unsafeShiftL` 2
  tileData = do
    i <- V.fromList [0..7]
    let (p1, p2) = if attr .&. 0x40 == 0x40
        then do
          let p1 = ((lo `unsafeShiftR` i) .&. 0x1) `unsafeShiftL` 0
          let p2 = ((hi `unsafeShiftR` i) .&. 0x1) `unsafeShiftL` 1
          (p1, p2)
        else do
          let p1 = ((lo `unsafeShiftL` i) .&. 0x80) `unsafeShiftR` 7
          let p2 = ((hi `unsafeShiftL` i) .&. 0x80) `unsafeShiftR` 6
          (p1, p2)

    pure $ fromIntegral $ atv .|. p1 .|. p2 :: V.Vector Word32
  tileData' = V.foldl' op 0 tileData
       where op acc i = (acc `unsafeShiftL` 4) .|. i

getSpriteAddress :: Int -> SpriteSize -> Word8 -> Word8 -> Emulator Word16
getSpriteAddress row size attr tile = case size of
  Normal -> do
    let row' = if attr .&. 0x80 == 0x80 then 7 - row else row
    table <- loadPpu spriteTable
    pure $ table + fromIntegral tile * 16 + fromIntegral row'
  Double -> do
    let row' = if attr .&. 0x80 == 0x80 then 15 - row else row
    let table = tile .&. 1
    let tile' = tile .&. 0xFE
    let (tile'', row'') = if row' > 7 then (tile' + 1, row' - 8) else (tile', row')
    pure $ (0x1000 * fromIntegral table) + fromIntegral tile'' * 16 + fromIntegral row''

isSpriteVisible :: Int -> SpriteSize -> Bool
isSpriteVisible row spriteSize = row >= 0 && row < h
  where
    h = case spriteSize of
          Normal -> 8
          Double -> 16

enterVBlank :: Emulator ()
enterVBlank = do
  storePpu verticalBlank True
  generateNMI <- loadPpu nmiEnabled
  when generateNMI $ storeCpu interrupt (Just NMI)

exitVBlank :: Emulator ()
exitVBlank = storePpu verticalBlank False

idle :: Emulator ()
idle = pure ()

getPaletteColor :: Word8 -> Color
getPaletteColor index = palette `V.unsafeIndex` fromIntegral index where
  palette = V.fromList [
    (0x66, 0x66, 0x66), (0x00, 0x2A, 0x88), (0x14, 0x12, 0xA7), (0x3B, 0x00, 0xA4),
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
