{-# LANGUAGE RecordWildCards #-}

module Emulator.Mapper.Mapper3 (
    Mapper3(..)
  , new
  , read
  , write
) where

import           Data.IORef
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Data.Bits ((.&.))
import           Emulator.Cartridge          as Cartridge
import           Emulator.Util
import           Prelude                     hiding (read)

data Mapper3 = Mapper3
  { cart     :: Cartridge
  , chrBank  :: IORef Int
  , prgBank1 :: Int
  , prgBank2 :: Int
  }

new :: Cartridge -> IO Mapper3
new cart @ Cartridge{..} = do
  let prgBanks = VUM.length (Cartridge.prgRom cart) `div` 0x4000
  chrBank <- newIORef 0
  let prgBank1 = 0
  let prgBank2 = prgBanks - 1
  pure $ Mapper3 cart chrBank prgBank1 prgBank2

read :: Mapper3 -> Word16 -> IO Word8
read (Mapper3 Cartridge {..} chrBank prgBank1 prgBank2) addr
  | addr' <  0x2000 = do
    chrBankV <- readIORef chrBank
    VUM.read chrRom ((chrBankV * 0x2000) + addr')
  | addr' >= 0xC000 = VUM.read prgRom ((prgBank2 * 0x4000) + (addr' - 0xC000))
  | addr' >= 0x8000 = VUM.read prgRom ((prgBank1 * 0x4000) + (addr' - 0x8000))
  | addr' >= 0x6000 = VUM.read sram (addr' - 0x6000)
  | otherwise = error $ "Erroneous cart read detected!: " ++ prettifyWord16 addr
  where addr' = fromIntegral addr

write :: Mapper3 -> Word16 -> Word8 -> IO ()
write (Mapper3 Cartridge {..} chrBank _ _) addr v
  | addr' < 0x2000 = do
    chrBankV <- readIORef chrBank
    VUM.write chrRom ((chrBankV * 0x2000) + addr') v
  | addr' >= 0x8000 = modifyIORef chrBank (const $ toInt v .&. 3)
  | addr' >= 0x6000 = VUM.write sram (addr' - 0x6000) v
  | otherwise = error $ "Erroneous cart write detected!" ++ prettifyWord16 addr
  where addr' = fromIntegral addr
