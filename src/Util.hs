module Util (
  -- * Functions
    prettifyWord16
  , makeW16
) where

import           Data.Bits   (shiftL, (.|.))
import           Data.Word   (Word16, Word8)
import           Text.Printf (printf)


prettifyWord16 :: Word16 -> String
prettifyWord16 = printf "%04x"

makeW16 :: Word8 -> Word8 -> Word16
makeW16 l h = (fromIntegral l :: Word16) .|. (fromIntegral h :: Word16) `shiftL` 8
