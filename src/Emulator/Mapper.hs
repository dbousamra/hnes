module Emulator.Mapper (
    Mapper(..)
  , new
) where

import           Data.Word
import           Emulator.Cartridge      as Cartridge
import           Emulator.Mapper.Mapper2 as Mapper2

data Mapper = Mapper
  { read   :: Word16 -> IO Word8
  , write  :: Word16 -> Word8 -> IO ()
  , mirror :: Int
  }

new :: Cartridge -> Mapper
new cart = case Cartridge.mapperType cart of
  0     -> mapper2 cart
  2     -> mapper2 cart
  other -> error $ "Unsupported mapper type " ++ show other


mapper2 :: Cartridge -> Mapper
mapper2 cart = Mapper (Mapper2.read cart) (Mapper2.write cart) (Cartridge.mirror cart)
