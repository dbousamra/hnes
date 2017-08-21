module Util (
  -- * Functions
    prettifyWord16
  , makeW16
  , splitW16
) where

import           Data.Bits   (shiftL, shiftR, (.&.), (.|.))
import           Data.Word   (Word16, Word8)
import           Text.Printf (printf)


prettifyWord16 :: Word16 -> String
prettifyWord16 = printf "%04x"

makeW16 :: Word8 -> Word8 -> Word16
makeW16 l h = (fromIntegral l :: Word16) .|. (fromIntegral h :: Word16) `shiftL` 8

splitW16 :: Word16 -> (Word8, Word8)
splitW16 w = (fromIntegral (w .&. 0xFF), fromIntegral (w `shiftR` 8))
