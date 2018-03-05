{-# LANGUAGE RecordWildCards #-}

module Emulator.Mapper.Mapper1 (
    Mapper1(..)
  , renderM1
  , new
  , read
  , write
) where

import           Data.Bits                   ((.&.), shiftL, shiftR, (.|.))
import           Data.IORef
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Cartridge          as Cartridge
import           Control.Monad (when)
import           Emulator.Util
import           Prelude                     hiding (read)

data Mapper1 = Mapper1
  { cart          :: Cartridge
  , shiftRegister :: IORef Word8
  , control       :: IORef Word8
  , prgMode       :: IORef Word8
  , chrMode       :: IORef Word8
  , prgBank       :: IORef Word8
  , chrBank0      :: IORef Word8
  , chrBank1      :: IORef Word8
  , chrOffsets    :: VUM.IOVector Int
  , prgOffsets    :: VUM.IOVector Int
  }

renderM1 :: Mapper1 -> IO String
renderM1 m @ Mapper1 {..} = do
  srv <- readIORef shiftRegister
  cv <- readIORef control
  prgModeV <- readIORef prgMode
  chrModeV <- readIORef chrMode
  prgBankV <- readIORef prgBank
  chrBank0V <- readIORef chrBank0
  chrBank1V <- readIORef chrBank1
  -- chrOffsetsV <- readIORef chrOffsets
  -- prgOffsetsV <- readIORef prgOffsets
  pure $ show (show srv)
    ++ " " ++ (show cv)
    ++ " " ++ (show prgModeV)
    ++ " " ++ (show chrModeV)
    ++ " " ++ (show prgBankV)
    ++ " " ++ (show chrBank0V)
    ++ " " ++ (show chrBank1V)

new :: Cartridge -> IO Mapper1
new cart = do
  shiftRegister <- newIORef 0x10
  control <- newIORef 0
  prgMode <- newIORef 0
  chrMode <- newIORef 0
  prgBank <- newIORef 0
  chrBank0 <- newIORef 0
  chrBank1 <- newIORef 0
  chrOffsets <- VUM.replicate 2 0x0
  prgOffsets <- VUM.replicate 2 0x0
  VUM.write chrOffsets 1 (getPrgBankOffset cart (-1))
  pure $ Mapper1
    cart
    shiftRegister
    control
    prgMode
    chrMode
    prgBank
    chrBank0
    chrBank1
    chrOffsets
    prgOffsets

read :: Mapper1 -> Word16 -> IO Word8
read m @ Mapper1 {..} addr
  | addr < 0x2000 = do
    let offset = addr `mod` 0x1000
    bank <- VUM.read chrOffsets (fromIntegral $ addr `div` 0x1000)
    VUM.read (chrRom cart) (bank + fromIntegral offset)
  | addr >= 0x8000 = do
    let addr'' = addr - 0x8000
    let offset = addr'' `div` 0x4000
    bank <- VUM.read prgOffsets (fromIntegral $ addr'' `div` 0x4000)
    VUM.read (prgRom cart) (bank + fromIntegral offset)
  | addr >= 0x6000 =
    VUM.read (sram cart) (fromIntegral addr - 0x6000)
  | otherwise = error $ "Erroneous cart read detected!: " ++ prettifyWord16 addr
  -- where addr' = fromIntegral addr

write :: Mapper1 -> Word16 -> Word8 -> IO ()
write m @ Mapper1 {..} addr v
  | addr < 0x2000 = do
    let offset = addr `mod` 0x1000
    bank <- VUM.read chrOffsets (fromIntegral $ addr `div` 0x1000)
    VUM.write (chrRom cart) (bank + fromIntegral offset) v
  | addr >= 0x8000 =
    loadRegister m addr v
  | addr >= 0x6000 =
    VUM.write (sram cart) ((fromIntegral addr) - 0x6000) v
  | otherwise = error $ "Erroneous cart write detected!" ++ prettifyWord16 addr
  -- where addr' = fromIntegral addr

getPrgBankOffset :: Cartridge -> Int -> Int
getPrgBankOffset Cartridge {..} index = do
  let index' = if index >= 0x80 then index - 0x100 else index
  let index'' = index' `mod` VUM.length prgRom `div` 0x4000
  let offset = index'' * 0x4000
  if offset < 0 then offset + VUM.length prgRom else offset

getChrBankOffset :: Cartridge -> Int -> Int
getChrBankOffset Cartridge {..} index = do
  let index' = if index >= 0x80 then index - 0x100 else index
  let index'' = index' `mod` VUM.length chrRom `div` 0x1000
  let offset = index'' * 0x1000
  if offset < 0 then offset + VUM.length chrRom else offset

loadRegister :: Mapper1 -> Word16 -> Word8 -> IO ()
loadRegister m @ Mapper1 {..} addr value =
  if value .&. 0x80 == 0x80 then do
    modifyIORef' shiftRegister (const 0x10)
    controlV <- readIORef control
    writeControl m (controlV .|. 0x0C)
  else do
    shiftRegisterV <- readIORef shiftRegister
    let complete = shiftRegisterV .&. 1 == 1
    modifyIORef' shiftRegister (`shiftR` 1)
    modifyIORef' shiftRegister (.|. (value .&. 1) `shiftL` 4)
    when complete $ do
      shiftRegisterV <- readIORef shiftRegister
      writeRegister m addr shiftRegisterV
      modifyIORef' shiftRegister (const 0x10)

writeRegister :: Mapper1 -> Word16 -> Word8 -> IO ()
writeRegister m addr v
  | addr <= 0x9FFF = writeControl m v
  | addr <= 0xBFFF = writeCHRBank0 m v
  | addr <= 0xDFFF = writeCHRBank1 m v
  | addr <= 0xFFFF = writePRGBank m v

writeControl :: Mapper1 -> Word8 -> IO ()
writeControl m @ Mapper1 {..} v = do
  modifyIORef' control (const v)
  modifyIORef' chrMode (const $ (v `shiftR` 4) .&. 1)
  modifyIORef' prgMode (const $ (v `shiftR` 2) .&. 3)
  modifyIORef' (mirror cart) (const $ toEnum $ fromIntegral $ v .&. 3)
  updateOffsets m

writeCHRBank0 :: Mapper1 -> Word8 -> IO ()
writeCHRBank0 m @ Mapper1 {..} v = do
  modifyIORef' chrBank0 (const v)
  updateOffsets m

writeCHRBank1 :: Mapper1 -> Word8 -> IO ()
writeCHRBank1 m @ Mapper1 {..} v = do
  modifyIORef' chrBank1 (const v)
  updateOffsets m

writePRGBank :: Mapper1 -> Word8 -> IO ()
writePRGBank m @ Mapper1 {..} v = do
  modifyIORef' prgBank (const $ v .&. 0x0F)
  updateOffsets m

updateOffsets :: Mapper1 -> IO ()
updateOffsets m @ Mapper1 {..} = do
  prgModeV <- readIORef prgMode
  chrModeV <- readIORef chrMode
  prgBankV <- readIORef prgBank
  chrBank0V <- readIORef chrBank0
  chrBank1V <- readIORef chrBank1

  case prgModeV of
    0 -> do
      VUM.write prgOffsets 0 (getPrgBankOffset cart (fromIntegral $ prgBankV .&. 0xFE))
      VUM.write prgOffsets 1 (getPrgBankOffset cart (fromIntegral $ prgBankV .|. 0x01))
    1 -> do
      VUM.write prgOffsets 0 (getPrgBankOffset cart (fromIntegral $ prgBankV .&. 0xFE))
      VUM.write prgOffsets 1 (getPrgBankOffset cart (fromIntegral $ prgBankV .|. 0x01))
    2 -> do
      VUM.write prgOffsets 0 0
      VUM.write prgOffsets 1 (getPrgBankOffset cart (fromIntegral prgBankV))
    3 -> do
      VUM.write prgOffsets 0 (getPrgBankOffset cart (fromIntegral prgBankV))
      VUM.write prgOffsets 1 (getPrgBankOffset cart (-1))

  case chrModeV of
    0 -> do
      VUM.write chrOffsets 0 (getChrBankOffset cart (fromIntegral $ chrBank0V .&. 0xFE))
      VUM.write chrOffsets 1 (getChrBankOffset cart (fromIntegral $ chrBank0V .|. 0x01))
    1 -> do
      VUM.write chrOffsets 0 (getChrBankOffset cart (fromIntegral chrBank0V))
      VUM.write chrOffsets 1 (getChrBankOffset cart (fromIntegral chrBank1V))