{-# LANGUAGE GADTs #-}

module Emulator.CPU (
    CPU(..)
  , newCPU
  , readRam
  , writeRam
) where

import           Control.Monad.ST
import           Data.STRef
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Prelude                     hiding (read)

data CPU s = CPU {
  pc  :: STRef       s Word16,
  sp  :: STRef       s Word8,
  a   :: STRef       s Word8,
  x   :: STRef       s Word8,
  y   :: STRef       s Word8,
  p   :: STRef       s Word8,
  ram :: VUM.MVector s Word8
}

newCPU :: ST s (CPU s)
newCPU = do
  pc <- newSTRef 0x0
  sp <- newSTRef 0xFD
  a <- newSTRef 0x0
  x <- newSTRef 0x0
  y <- newSTRef 0x0
  p <- newSTRef 0x24 -- should this be 0x34?
  ram <- VUM.replicate 65536 0x0
  pure $ CPU pc sp a x y p ram

readRam :: CPU s -> Word16 -> ST s Word8
readRam cpu addr = VUM.read (ram cpu) (fromIntegral addr `mod` 0x0800)

writeRam :: CPU s -> Word16 -> Word8 -> ST s ()
writeRam cpu addr = VUM.write (ram cpu) (fromIntegral addr `mod` 0x0800)
