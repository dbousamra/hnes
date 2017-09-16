{-# LANGUAGE GADTs #-}

module Emulator.CPU (
    CPU(..)
  , newCPU
  , read
  , write
  , readRam
  , writeRam
) where

import           Control.Monad.ST
import           Data.STRef
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Address            (CpuAddress (..))
import           Prelude                     hiding (read)

data CPU s = CPU {
  pc     :: STRef       s Word16,
  sp     :: STRef       s Word8,
  a      :: STRef       s Word8,
  x      :: STRef       s Word8,
  y      :: STRef       s Word8,
  p      :: STRef       s Word8,
  ram    :: VUM.MVector s Word8,
  cycles :: STRef       s Int
}

newCPU :: ST s (CPU s)
newCPU = do
  pc <- newSTRef 0x0
  sp <- newSTRef 0xFD
  a <- newSTRef 0x0
  x <- newSTRef 0x0
  y <- newSTRef 0x0
  p <- newSTRef 0x24 -- should this be 0x34?
  p <- newSTRef 0x24 -- should this be 0x34?
  ram <- VUM.replicate 65536 0x0
  cycles <- newSTRef 0
  pure $ CPU pc sp a x y p ram cycles

write :: CPU s -> CpuAddress a -> a -> ST s ()
write cpu addr v = case addr of
  Pc        -> modifySTRef' (pc cpu) (const v)
  Sp        -> modifySTRef' (sp cpu) (const v)
  A         -> modifySTRef' (a cpu) (const v)
  X         -> modifySTRef' (x cpu) (const v)
  Y         -> modifySTRef' (y cpu) (const v)
  P         -> modifySTRef' (p cpu) (const v)
  CpuCycles -> modifySTRef' (cycles cpu) (const v)

read :: CPU s -> CpuAddress a -> ST s a
read cpu addr = case addr of
  Pc        -> readSTRef $ pc cpu
  Sp        -> readSTRef $ sp cpu
  A         -> readSTRef $ a cpu
  X         -> readSTRef $ x cpu
  Y         -> readSTRef $ y cpu
  P         -> readSTRef $ p cpu
  CpuCycles -> readSTRef $ cycles cpu

readRam :: CPU s -> Word16 -> ST s Word8
readRam cpu addr = VUM.read (ram cpu) (fromIntegral addr `mod` 0x0800)

writeRam :: CPU s -> Word16 -> Word8 -> ST s ()
writeRam cpu addr = VUM.write (ram cpu) (fromIntegral addr `mod` 0x0800)
