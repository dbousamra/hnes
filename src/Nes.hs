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
  p      :: STRef       s Word8,
  x      :: STRef       s Word8,
  y      :: STRef       s Word8
}

-- GADTs are used to represent addressing
data Address a where
  Pc    :: Address Word16
  Sp    :: Address Word8
  P     :: Flag -> Address Bool
  A     :: Address Word8
  X     :: Address Word8
  Y     :: Address Word8
  Ram8  :: Word16 -> Address Word8
  Ram16 :: Word16 -> Address Word16

data Flag
  = FN
  | FV
  | F1
  | FB
  | FD
  | FI
  | FZ
  | FC
  deriving (Enum)

new :: Cartridge -> ST s (Nes s)
new cart = do
  let mapper = mapper0 cart
  mem <- VUM.replicate 65536 0x0
  pc <- newSTRef 0x0
  sp <- newSTRef 0xFD
  p <- newSTRef 0x34
  a <- newSTRef 0x0
  x <- newSTRef 0x0
  y <- newSTRef 0x0
  pure $ Nes mem mapper pc sp p a x y

load :: Nes s -> Address a -> ST s a
load nes addr = case addr of
  Pc        -> loadPc nes
  Sp        -> loadSp nes
  A         -> loadA nes
  X         -> loadX nes
  Y         -> loadY nes
  (P flag)  -> loadP nes flag
  (Ram8 r)  -> loadRam8 nes r
  (Ram16 r) -> loadRam16 nes r

loadPc :: Nes s -> ST s Word16
loadPc nes = readSTRef (pc nes)

loadSp :: Nes s -> ST s Word8
loadSp nes = readSTRef (sp nes)

loadA :: Nes s -> ST s Word8
loadA nes = readSTRef (a nes)

loadX :: Nes s -> ST s Word8
loadX nes = readSTRef (x nes)

loadY :: Nes s -> ST s Word8
loadY nes = readSTRef (y nes)

loadP :: Nes s -> Flag -> ST s Bool
loadP nes flag = do
  v <- readSTRef (p nes)
  pure $ testBit v (7 - fromEnum flag)

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
  Pc        -> storePc nes v
  Sp        -> storeSp nes v
  A         -> storeA nes v
  X         -> storeX nes v
  Y         -> storeY nes v
  (P flag)  -> storeP nes flag v
  (Ram8 r)  -> storeRam8 nes r v
  (Ram16 r) -> storeRam16 nes r v

storePc :: Nes s -> Word16 -> ST s ()
storePc nes v = modifySTRef' (pc nes) (const v)

storeSp :: Nes s -> Word8 -> ST s ()
storeSp nes v = modifySTRef' (sp nes) (const v)

storeA :: Nes s -> Word8 -> ST s ()
storeA nes v  = modifySTRef' (a nes) (const v)

storeX :: Nes s -> Word8 -> ST s ()
storeX nes v  = modifySTRef' (x nes) (const v)

storeY :: Nes s -> Word8 -> ST s ()
storeY nes v  = modifySTRef' (y nes) (const v)

storeP :: Nes s -> Flag -> Bool -> ST s ()
storeP nes flag b = do
  v <- readSTRef (p nes)
  modifySTRef' (p nes) (const $ opBit v (7 - fromEnum flag))
  where opBit = if b then setBit else clearBit

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
