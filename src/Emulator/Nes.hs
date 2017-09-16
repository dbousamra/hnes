{-# LANGUAGE GADTs #-}

module Emulator.Nes (
    Nes(..)
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
import           Emulator.Address
import           Emulator.Cartridge
import           Emulator.CPU                as CPU
import           Emulator.Mapper
import           Emulator.PPU                as PPU
import           Emulator.Util
import           Prelude                     hiding (replicate)

data Nes s = Nes {
  cpu    :: CPU s,
  ppu    :: PPU s,
  mapper :: Mapper
}

new :: Cartridge -> ST s (Nes s)
new cart = do
  mapper <- pure $ mapper0 cart
  cpu <- newCPU
  ppu <- newPPU
  pure $ Nes cpu ppu mapper

load :: Nes s -> Address a -> ST s a
load nes addr = case addr of
  (CpuAddress r) -> CPU.read (cpu nes) r
  (PpuAddress r) -> PPU.read (ppu nes) r
  (Ram8 r)       -> loadRam8 nes r
  (Ram16 r)      -> loadRam16 nes r

loadRam8 :: Nes s -> Word16 -> ST s Word8
loadRam8 nes addr
  | addr < 0x2000 = CPU.readRam (cpu nes) addr
  | addr < 0x4000 = PPU.readRegister (ppu nes) addr
  | addr >= 0x4000 && addr <= 0x4017 = error "IO read not implemented!"
  | addr >= 0x6000 && addr <= 0xFFFF = pure $ readRom (mapper nes) addr
  | otherwise = error "Erroneous read detected!"

loadRam16 :: Nes s -> Word16 -> ST s Word16
loadRam16 nes addr = do
  lo <- load nes (Ram8 addr)
  hi <- load nes (Ram8 $ addr + 1)
  pure $ makeW16 lo hi

store :: Nes s -> Address a -> a -> ST s ()
store nes addr v = case addr of
  (CpuAddress r) -> CPU.write (cpu nes) r v
  (PpuAddress r) -> PPU.write (ppu nes) r v
  (Ram8 r)       -> storeRam8 nes r v
  (Ram16 r)      -> storeRam16 nes r v

storeRam8 :: Nes s -> Word16 -> Word8 -> ST s ()
storeRam8 nes r v
  | r < 0x2000 = CPU.writeRam (cpu nes) r v
  | r < 0x4000 = PPU.writeRegister (ppu nes) r v
  | r >= 0x4000 && r <= 0x4017 = pure ()
  | r >= 0x4020 && r <= 0xFFFF = error "Cannot write to cart space"
  | otherwise = error "Erroneous write detected!"

storeRam16 :: Nes s -> Word16 -> Word16 -> ST s ()
storeRam16 nes addr v = do
  let (lo, hi) = splitW16 v
  store nes (Ram8 addr) lo
  store nes (Ram8 $ addr + 1) hi
