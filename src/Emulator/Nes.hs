{-# LANGUAGE GADTs #-}

module Emulator.Nes (
    Nes(..)
  , Address(..)
  , Flag(..)
  , new
  , load
  , store
) where

import           Control.Monad.ST
import           Data.Bits                   ((.&.))
import qualified Data.ByteString             as BS
import           Data.STRef
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Cartridge
import           Emulator.CPU                as CPU
import           Emulator.Mapper
import           Emulator.PPU                as PPU
import           Emulator.Util
import           Prelude                     hiding (replicate)

data Nes s = Nes {
  cpu    :: CPU s,
  ppu    :: PPU s,
  ram    :: VUM.MVector s Word8,
  mapper :: Mapper
}

-- GADTs are used to represent addressing
data Address a where
  Pc    :: Address Word16
  Sp    :: Address Word8
  A     :: Address Word8
  X     :: Address Word8
  Y     :: Address Word8
  P     :: Address Word8
  Ram8  :: Word16 -> Address Word8
  Ram16 :: Word16 -> Address Word16

data Flag
  = Negative
  | Overflow
  | Unused
  | Break
  | Decimal
  | Interrupt
  | Zero
  | Carry
  deriving (Enum)

new :: Cartridge -> ST s (Nes s)
new cart = do
  mapper <- pure $ mapper0 cart
  cpu <- newCPU
  ppu <- newPPU
  ram <- VUM.replicate 65536 0x0
  pure $ Nes cpu ppu ram mapper

load :: Nes s -> Address a -> ST s a
load nes addr = case addr of
  Pc        -> readSTRef $ (pc . cpu) nes
  Sp        -> readSTRef $ (sp . cpu) nes
  A         -> readSTRef $ (a . cpu) nes
  X         -> readSTRef $ (x . cpu) nes
  Y         -> readSTRef $ (y . cpu) nes
  P         -> readSTRef $ (p . cpu) nes
  (Ram8 r)  -> loadRam8 nes r
  (Ram16 r) -> loadRam16 nes r

loadRam8 :: Nes s -> Word16 -> ST s Word8
loadRam8 nes r
  | r < 0x2000 = VUM.read (ram nes) (fromIntegral r `mod` 0x0800)
  | r < 0x4000 = PPU.read (ppu nes) r
  | r >= 0x2008 && r <= 0x3FFF = error "PPU read not implemented!"
  | r >= 0x4000 && r <= 0x4017 = error "IO read not implemented!"
  | r >= 0x6000 && r <= 0xFFFF = pure $ readRom (mapper nes) r
  | otherwise = error "Erroneous read detected!"

loadRam16 :: Nes s -> Word16 -> ST s Word16
loadRam16 nes r = do
  let addr = fromIntegral r
  lo <- load nes (Ram8 addr)
  hi <- load nes (Ram8 $ addr + 1)
  pure $ makeW16 lo hi

store :: Nes s -> Address a -> a -> ST s ()
store nes addr v = case addr of
  Pc        -> modifySTRef' ((pc . cpu) nes) (const v)
  Sp        -> modifySTRef' ((sp . cpu) nes) (const v)
  A         -> modifySTRef' ((a . cpu) nes) (const v)
  X         -> modifySTRef' ((x . cpu) nes) (const v)
  Y         -> modifySTRef' ((y . cpu) nes) (const v)
  P         -> modifySTRef' ((p . cpu) nes) (const v)
  (Ram8 r)  -> storeRam8 nes r v
  (Ram16 r) -> storeRam16 nes r v

storeRam8 :: Nes s -> Word16 -> Word8 -> ST s ()
storeRam8 nes r v
  | r < 0x2000 = VUM.write (ram nes) (fromIntegral r `mod` 0x0800) v
  | r < 0x4000 = PPU.write (ppu nes) (0x2000 + r `mod` 8) v
  | r >= 0x2008 && r <= 0x3FFF = error "PPU write not implemented!"
  | r >= 0x4000 && r <= 0x4017 = pure ()
  | r >= 0x4020 && r <= 0xFFFF = error "Cannot write to cart space"
  | otherwise = error "Erroneous write detected!"

storeRam16 :: Nes s -> Word16 -> Word16 -> ST s ()
storeRam16 nes r v = do
  let addr = fromIntegral r
  let (lo, hi) = splitW16 v
  store nes (Ram8 addr) lo
  store nes (Ram8 $ addr + 1) hi
