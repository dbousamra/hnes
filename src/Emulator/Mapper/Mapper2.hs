{-# LANGUAGE RecordWildCards #-}

module Emulator.Mapper.Mapper2 (
    read
  , write
) where

import           Data.IORef
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Cartridge          as Cartridge
import           Emulator.Util
import           Prelude                     hiding (read)

read :: Cartridge -> Word16 -> IO Word8
read Cartridge {..} addr
  | addr' <  0x2000 = VUM.unsafeRead chrRom addr'
  | addr' >= 0xC000 = do
    prgBank2V <- readIORef prgBank2
    VUM.unsafeRead prgRom ((prgBank2V * 0x4000) + (addr' - 0xC000))
  | addr' >= 0x8000 = do
    prgBank1V <- readIORef prgBank1
    VUM.unsafeRead prgRom ((prgBank1V * 0x4000) + (addr' - 0x8000))
  | addr' >= 0x6000 = VUM.unsafeRead sram (addr' - 0x6000)
  | otherwise = error $ "Erroneous cart read detected!: " ++ prettifyWord16 addr
  where addr' = fromIntegral addr

write :: Cartridge -> Word16 -> Word8 -> IO ()
write Cartridge {..} addr v
  | addr' < 0x2000 = VUM.unsafeWrite chrRom addr' v
  | addr' >= 0x8000 = modifyIORef prgBank1 (const $ toInt v)
  | addr' >= 0x6000 = VUM.unsafeWrite sram (addr' - 0x6000) v
  | otherwise = error $ "Erroneous cart write detected!" ++ prettifyWord16 addr
  where addr' = fromIntegral addr
