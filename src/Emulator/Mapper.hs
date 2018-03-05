module Emulator.Mapper (
    Mapper(..)
  , new
) where

import           Control.Monad.IO.Class
import           Data.Word
import           Emulator.Cartridge
import qualified Emulator.Mapper.Mapper1 as Mapper1
import qualified Emulator.Mapper.Mapper2 as Mapper2
import qualified Emulator.Mapper.Mapper3 as Mapper3
import qualified Emulator.Mapper.Mapper7 as Mapper7

data Mapper = Mapper
  { read  :: Word16 -> IO Word8
  , write :: Word16 -> Word8 -> IO ()
  }

new :: Cartridge -> IO Mapper
new cart = case mapperType cart of
  0     -> mapper2 cart
  1     -> mapper1 cart
  2     -> mapper2 cart
  3     -> mapper3 cart
  7     -> mapper7 cart
  66    -> mapper2 cart
  other -> error $ "Unsupported mapper type " ++ show other

mapper1 :: Cartridge -> IO Mapper
mapper1 cart = do
  m <- Mapper1.new cart
  pure $ Mapper (Mapper1.read m) (Mapper1.write m)

mapper2 :: Cartridge -> IO Mapper
mapper2 cart = do
  m <- Mapper2.new cart
  pure $ Mapper (Mapper2.read m) (Mapper2.write m)

mapper3 :: Cartridge -> IO Mapper
mapper3 cart = do
  m <- Mapper3.new cart
  pure $ Mapper (Mapper3.read m) (Mapper3.write m)

mapper7 :: Cartridge -> IO Mapper
mapper7 cart = do
  m <- Mapper7.new cart
  pure $ Mapper (Mapper7.read m) (Mapper7.write m)
