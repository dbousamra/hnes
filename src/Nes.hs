{-# LANGUAGE GADTs #-}

module Nes (
  -- * Types
    Nes(..)
  , Address(..)
  , Flag(..)
  -- * Functions
  , new
  , load
  , store
) where

import           Cartridge
import           Constants
import           Control.Monad.ST
import           Data.Bits                   (clearBit, setBit, testBit, (.&.))
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

data Nes s = Nes {
  ram    :: VUM.MVector s Word8,
  mapper :: Mapper      s,
  pc     :: STRef       s Word16,
  sp     :: STRef       s Word8,
  a      :: STRef       s Word8,
  x      :: STRef       s Word8,
  y      :: STRef       s Word8,
  p      :: STRef       s Word8
}

-- GADTs are used to represent addressing
data Address a where
  Pc    :: Address Word16
  Sp    :: Address Word8
  A     :: Address Word8
  X     :: Address Word8
  Y     :: Address Word8
  -- P     :: Flag -> Address Bool
  P     :: Address Word8
  Ram8  :: Word16 -> Address Word8
  Ram16 :: Word16 -> Address Word16

data Flag
  = Negative
  | Overflow
  | F1
  | FB
  | Decimal
  | Interrupt
  | Zero
  | Carry
  deriving (Enum)

new :: Cartridge -> ST s (Nes s)
new cart = do
  let mapper = mapper0 cart
  ram <- VUM.replicate 65536 0x0
  pc <- newSTRef 0x0
  sp <- newSTRef 0xFD
  a <- newSTRef 0x0
  x <- newSTRef 0x0
  y <- newSTRef 0x0
  p <- newSTRef 0x24 -- should this be 0x34?
  pure $ Nes ram mapper pc sp a x y p

load :: Nes s -> Address a -> ST s a
load nes addr = case addr of
  Pc        -> readSTRef (pc nes)
  Sp        -> readSTRef (sp nes)
  A         -> readSTRef (a nes)
  X         -> readSTRef (x nes)
  Y         -> readSTRef (y nes)
  P         -> readSTRef (p nes)
  (Ram8 r)  -> loadRam8 nes r
  (Ram16 r) -> loadRam16 nes r

loadRam8 :: Nes s -> Word16 -> ST s Word8
loadRam8 nes r
  | r >= cpuRamBegin      && r <= cpuRamEnd = VUM.read (ram nes) addr
  | r >= ramMirrorsBegin  && r <= ramMirrorsEnd = VUM.read (ram nes) (addr .&. 0x07FF)
  | r >= ppuRegisterBegin && r <= ppuRegisterEnd = error "PPU read not implemented!"
  | r >= ppuMirrorsBegin  && r <= ppuMirrorsEnd = error "PPU read not implemented!"
  | r >= ioRegistersBegin && r <= ioRegistersEnd = error "IO read not implemented!"
  | r >= cartSpaceBegin   && r <= cartSpaceEnd = pure $ readRom (mapper nes) r
  | otherwise = error "Erroneous read detected!"
  where addr = fromIntegral r

loadRam16 :: Nes s -> Word16 -> ST s Word16
loadRam16 nes r = do
  let addr = fromIntegral r
  lo <- load nes (Ram8 addr)
  hi <- load nes (Ram8 $ addr + 1)
  pure $ makeW16 lo hi

store :: Nes s -> Address a -> a -> ST s ()
store nes addr v = case addr of
  Pc        -> modifySTRef' (pc nes) (const v)
  Sp        -> modifySTRef' (sp nes) (const v)
  A         -> modifySTRef' (a nes) (const v)
  X         -> modifySTRef' (x nes) (const v)
  Y         -> modifySTRef' (y nes) (const v)
  P         -> modifySTRef' (p nes) (const v)
  (Ram8 r)  -> storeRam8 nes r v
  (Ram16 r) -> storeRam16 nes r v

storeRam8 :: Nes s -> Word16 -> Word8 -> ST s ()
storeRam8 nes r v
  | r >= cpuRamBegin      && r <= cpuRamEnd = VUM.write (ram nes) addr v
  | r >= ramMirrorsBegin  && r <= ramMirrorsEnd = VUM.write (ram nes) (addr `mod` 0x07FF) v
  | r >= ppuRegisterBegin && r <= ppuRegisterEnd = error "PPU write not implemented!"
  | r >= ppuMirrorsBegin  && r <= ppuMirrorsEnd = error "PPU write not implemented!"
  | r >= ioRegistersBegin && r <= ioRegistersEnd = error "IO write not implemented!"
  | r >= cartSpaceBegin   && r <= cartSpaceEnd = error "Cannot write to cart space"
  | otherwise = error "Erroneous write detected!"
  where addr = (fromIntegral r)

storeRam16 :: Nes s -> Word16 -> Word16 -> ST s ()
storeRam16 nes r v = do
  let addr = fromIntegral r
  let (lo, hi) = splitW16 v
  store nes (Ram8 addr) lo
  store nes (Ram8 $ addr + 1) hi

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
