{-# LANGUAGE GADTs #-}

module Nes (
  -- * Types
    Nes(..)
  , Address(..)
  -- * Functions
  , new
  , load
  , store
  -- , render
) where

import           Cartridge
import           Control.Monad.ST
import           Data.Bits                   ((.&.))
import qualified Data.ByteString             as BS
import           Data.STRef                  (STRef, modifySTRef', newSTRef,
                                              readSTRef)
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Prelude                     hiding (replicate)
import           Util

data Mapper s = Mapper {
  cart    :: Cartridge,
  readRom :: Word16 -> Word8
}

mapper0 :: Cartridge -> Mapper s
mapper0 cart = Mapper cart readRom where
  readRom r
    | addr < 0x2000 = BS.index (chrRom cart) addr
    | addr >= 0xC000 = BS.index (prgRom cart) ((prgBank2 * 0x4000) + (addr - 0xC000))
    | addr >= 0x8000 = BS.index (prgRom cart) ((prgBank1 * 0x4000) + (addr - 0x8000))
    | addr >= 0x6000 = BS.index (prgRom cart) (addr - 0x6000)
    | otherwise = error $ "Erroneous mapper0 read detected!: " ++ prettifyWord16 r
    where
      addr = fromIntegral r
      prgBanks = (BS.length (prgRom cart)) `div` 0x4000
      prgBank1 = 0
      prgBank2 = prgBanks - 1


data Nes s = Nes {
  ram    :: VUM.MVector s Word8,
  mapper :: Mapper s,
  pc     :: STRef       s Word16,
  sp     :: STRef       s Word8
}

-- GADTs are used to represent addressing
data Address a where
  Pc    :: Address Word16
  Sp    :: Address Word8
  P     :: Address Word8
  X     :: Address Word8
  Y     :: Address Word8
  Ram8  :: Word16 -> Address Word8
  Ram16 :: Word16 -> Address Word16

new :: Cartridge -> ST s (Nes s)
new cart = do
  let mapper = mapper0 cart
  mem <- VUM.replicate 65536 0
  pc <- newSTRef 0
  sp <- newSTRef 0xFD
  pure $ Nes mem mapper pc sp

load :: Nes s -> Address a -> ST s a
load nes Pc       = readSTRef (pc nes)
load nes Sp       = readSTRef (sp nes)
load nes (Ram8 r)
  -- | r >= cpuRamBegin      || r <= cpuRamEnd = VUM.read (ram nes) addr
  -- | r >= ramMirrorsBegin  || r <= ramMirrorsEnd = VUM.read (ram nes) (addr .&. 0x07FF)
  -- | r >= ppuRegisterBegin || r <= ppuRegisterEnd = error "PPU read not implemented!"
  -- | r >= ppuMirrorsBegin  || r <= ppuMirrorsEnd = error "PPU read not implemented!"
  -- | r >= ioRegistersBegin || r <= ioRegistersEnd = error "IO read not implemented!"
  | r >= cartSpaceBegin   || r <= cartSpaceEnd = pure $ readRom (mapper nes) r
  | otherwise = error "Erroneous read detected!"
  where addr = fromIntegral r
load nes (Ram16 r) = do
  let idx = fromIntegral r
  l <- VUM.read (ram nes) idx
  h <- VUM.read (ram nes) (idx + 1)
  pure $ makeW16 l h

store :: Nes s -> Address a -> a -> ST s ()
store nes Pc v       = modifySTRef' (pc nes) (const v)
store nes Sp v       = modifySTRef' (sp nes) (const v)
store nes (Ram8 r) v = VUM.write (ram nes) (fromIntegral r) v
store nes (Ram16 r) v = do
  let idx = fromIntegral r
  let (l, h) = splitW16 v
  VUM.write (ram nes) idx l
  VUM.write (ram nes) (idx + 1) h

cpuRamBegin :: Word16
cpuRamBegin = 0x0000

cpuRamEnd :: Word16
cpuRamEnd = 0x07FF

ramMirrorsBegin :: Word16
ramMirrorsBegin = 0x0800

ramMirrorsEnd :: Word16
ramMirrorsEnd = 0x1FFF

ppuRegisterBegin :: Word16
ppuRegisterBegin = 0x2000

ppuRegisterEnd :: Word16
ppuRegisterEnd= 0x2007

ppuMirrorsBegin :: Word16
ppuMirrorsBegin = 0x2008

ppuMirrorsEnd :: Word16
ppuMirrorsEnd= 0x3FFF

ioRegistersBegin :: Word16
ioRegistersBegin = 0x4000

ioRegistersEnd :: Word16
ioRegistersEnd= 0x4017

cartSpaceBegin :: Word16
cartSpaceBegin = 0x4020

cartSpaceEnd :: Word16
cartSpaceEnd= 0xFFFF

