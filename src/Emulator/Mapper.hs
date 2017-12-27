module Emulator.Mapper (
    Mapper(..)
  , new
) where

import           Data.Word
import           Emulator.Cartridge      as Cartridge
import qualified Emulator.Mapper.Mapper2 as Mapper2

data Mapper = Mapper
  { read   :: Word16 -> IO Word8
  , write  :: Word16 -> Word8 -> IO ()
  , mirror :: Int
  }

new :: Cartridge -> IO Mapper
new cart = case Cartridge.mapperType cart of
  0     -> mapper2 cart
  2     -> mapper2 cart
  other -> error $ "Unsupported mapper type " ++ show other

mapper2 :: Cartridge -> IO Mapper
mapper2 cart = do
  m2 <- Mapper2.new cart
  let r = Mapper2.read m2
  let w = Mapper2.write m2
  let m = Cartridge.mirror cart
  pure $ Mapper r w m

