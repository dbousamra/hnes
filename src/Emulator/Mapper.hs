module Emulator.Mapper (
    Mapper(..)
  , mapper0
) where

import qualified Data.ByteString    as BS
import           Data.Word
import           Emulator.Cartridge
import           Emulator.Util      (prettifyWord16)

data Mapper = Mapper {
  cart    :: Cartridge,
  readRom :: Word16 -> Word8
}

mapper0 :: Cartridge -> Mapper
mapper0 cart = Mapper cart readRom where
  readRom r
    | addr <  0x2000 = BS.index (chrRom cart) addr
    | addr >= 0xC000 = BS.index (prgRom cart) ((prgBank2 * 0x4000) + (addr - 0xC000))
    | addr >= 0x8000 = BS.index (prgRom cart) ((prgBank1 * 0x4000) + (addr - 0x8000))
    | addr >= 0x6000 = BS.index (prgRom cart) (addr - 0x6000)
    | otherwise = error $ "Erroneous mapper0 read detected!: " ++ prettifyWord16 r
    where
      addr = fromIntegral r
      prgBanks = BS.length (prgRom cart) `div` 0x4000
      prgBank1 = 0
      prgBank2 = prgBanks - 1
