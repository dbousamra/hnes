module Util (
  -- * Functions
    prettifyWord16
  , prettifyWord8
  , makeW16
  , splitW16
  , sliceBS
) where

import           Data.Bits       (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import           Data.Word       (Word16, Word8)
import           Text.Printf     (printf)

prettifyWord16 :: Word16 -> String
prettifyWord16 = printf "0x%04x"

prettifyWord8 :: Word8 -> String
prettifyWord8 = printf "0x%02x"

makeW16 :: Word8 -> Word8 -> Word16
makeW16 l h = (fromIntegral l :: Word16) .|. (fromIntegral h :: Word16) `shiftL` 8

splitW16 :: Word16 -> (Word8, Word8)
splitW16 w = (fromIntegral (w .&. 0xFF), fromIntegral (w `shiftR` 8))

sliceBS :: Int -> Int -> BS.ByteString -> BS.ByteString
sliceBS from to xs = BS.take (to - from) (BS.drop from xs)


