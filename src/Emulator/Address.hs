{-# LANGUAGE GADTs #-}

module Emulator.Address (
    Address(..)
  , Flag(..)
) where

import           Data.Word

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
