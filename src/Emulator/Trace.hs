module Emulator.Trace (
  -- * Types
    Trace(..)
  -- * Functions
  , renderTrace
) where

import           Data.Word
import           Emulator.Opcode
import           Text.Printf

data Trace = Trace {
  pc :: Word16,
  sp :: Word8,
  a  :: Word8,
  x  :: Word8,
  y  :: Word8,
  p  :: Word8,
  op :: Opcode,
  a0 :: Word8,
  a1 :: Word8,
  a2 :: Word8
} deriving (Eq)

instance Show Trace where
  show = renderTrace

renderTrace :: Trace -> String
renderTrace (Trace pcv spv av xv yv pv op @ (Opcode _ mnem _) a0 a1 a2) =
  executionPortion ++ registerPortion where
    instrLength = instructionLength op
    a0R = printf "%02X" a0 :: String
    a1R = if instrLength < 2 then "  " else printf "%02X" a1
    a2R = if instrLength < 3 then "  " else printf "%02X" a2
    name = show mnem
    executionPortion = printf "%4X  %s %s %s  %s %28s" pcv a0R a1R a2R name ""
    registerPortion = printf "A:%02X X:%02X Y:%02X P:%02X SP:%02X" av xv yv pv spv
