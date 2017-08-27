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
import           Constants
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
    | addr <  0x2000 = BS.index (chrRom cart) addr
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
  mapper :: Mapper      s,
  pc     :: STRef       s Word16,
  sp     :: STRef       s Word8,
  x      :: STRef       s Word8,
  y      :: STRef       s Word8
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
  mem <- VUM.replicate 65536 0x0
  pc <- newSTRef 0x0
  sp <- newSTRef 0xFD
  x <- newSTRef 0x0
  y <- newSTRef 0x0
  pure $ Nes mem mapper pc sp x y

load :: Nes s -> Address a -> ST s a
load nes Pc       = readSTRef (pc nes)
load nes Sp       = readSTRef (sp nes)
load nes X        = readSTRef (x nes)
load nes Y        = readSTRef (y nes)
load nes (Ram8 r)
  | r >= cpuRamBegin      && r <= cpuRamEnd = VUM.read (ram nes) addr
  | r >= ramMirrorsBegin  && r <= ramMirrorsEnd = VUM.read (ram nes) (addr .&. 0x07FF)
  | r >= ppuRegisterBegin && r <= ppuRegisterEnd = error "PPU read not implemented!"
  | r >= ppuMirrorsBegin  && r <= ppuMirrorsEnd = error "PPU read not implemented!"
  | r >= ioRegistersBegin && r <= ioRegistersEnd = error "IO read not implemented!"
  | r >= cartSpaceBegin   && r <= cartSpaceEnd = pure $ readRom (mapper nes) r
  | otherwise = error "Erroneous read detected!"
  where addr = fromIntegral r
load nes (Ram16 r) = do
  let addr = fromIntegral r
  lo <- load nes (Ram8 addr)
  hi <- load nes (Ram8 $ addr + 1)
  pure $ makeW16 lo hi

store :: Nes s -> Address a -> a -> ST s ()
store nes Pc v = modifySTRef' (pc nes) (const v)
store nes Sp v = modifySTRef' (sp nes) (const v)
store nes X v  = modifySTRef' (x nes) (const v)
store nes Y v  = modifySTRef' (y nes) (const v)
store nes (Ram8 r) v
  | r >= cpuRamBegin      && r <= cpuRamEnd = VUM.write (ram nes) addr v
  | r >= ramMirrorsBegin  && r <= ramMirrorsEnd = VUM.write (ram nes) (addr `mod` 0x07FF) v
  | r >= ppuRegisterBegin && r <= ppuRegisterEnd = error "PPU write not implemented!"
  | r >= ppuMirrorsBegin  && r <= ppuMirrorsEnd = error "PPU write not implemented!"
  | r >= ioRegistersBegin && r <= ioRegistersEnd = error "IO write not implemented!"
  | r >= cartSpaceBegin   && r <= cartSpaceEnd = error "Cannot write to cart space"
  | otherwise = error "Erroneous write detected!"
  where addr = (fromIntegral r)
store nes (Ram16 r) v = do
  let addr = fromIntegral r
  let (lo, hi) = splitW16 v
  store nes (Ram8 addr) lo
  store nes (Ram8 $ addr + 1) hi
