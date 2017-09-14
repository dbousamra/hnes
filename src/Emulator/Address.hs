{-# LANGUAGE GADTs #-}

module Emulator.Address (
    Address(..)
  , CpuAddress(..)
  , Flag(..)
) where

import           Data.Word

data CpuAddress a where
  Pc    :: CpuAddress Word16
  Sp    :: CpuAddress Word8
  A     :: CpuAddress Word8
  X     :: CpuAddress Word8
  Y     :: CpuAddress Word8
  P     :: CpuAddress Word8

-- GADTs are used to represent addressing
data Address a where
  CpuAddress :: CpuAddress a -> Address a
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
