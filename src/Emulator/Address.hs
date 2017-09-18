{-# LANGUAGE GADTs #-}

module Emulator.Address (
    Address(..)
  , CpuAddress(..)
  , PpuAddress(..)
  , Flag(..)
) where

import           Data.Vector.Unboxed
import           Data.Word

-- GADTs are used to represent addressing
data CpuAddress a where
  Pc        :: CpuAddress Word16
  Sp        :: CpuAddress Word8
  A         :: CpuAddress Word8
  X         :: CpuAddress Word8
  Y         :: CpuAddress Word8
  P         :: CpuAddress Word8
  CpuCycles :: CpuAddress Int

data PpuAddress a where
  PpuCycles :: PpuAddress Int
  Scanline  :: PpuAddress Int
  VBlank    :: PpuAddress Bool
  Screen    :: PpuAddress (Vector Word8)

data Address a where
  CpuAddress :: CpuAddress a -> Address a
  PpuAddress :: PpuAddress a -> Address a
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
