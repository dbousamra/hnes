module Util (
  -- * Functions
    prettifyWord16
  , prettifyWord8
  , makeW16
  , toWord16
  , firstNibble
  , splitW16
  , sliceBS
) where

import           Data.Bits        (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString  as BS
import           Data.Word        (Word16, Word8)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Printf      (printf)

prettifyWord16 :: Word16 -> String
prettifyWord16 = printf "%04X"

prettifyWord8 :: Word8 -> String
prettifyWord8 = printf "%02X"

makeW16 :: Word8 -> Word8 -> Word16
makeW16 lo hi = (toWord16 lo) .|. (toWord16 hi) `shiftL` 8

toWord16 :: Word8 -> Word16
toWord16 = fromIntegral

splitW16 :: Word16 -> (Word8, Word8)
splitW16 w = (fromIntegral (w .&. 0xFF), fromIntegral (w `shiftR` 8))

firstNibble :: Word16 -> Word16
firstNibble = toWord16 . fst . splitW16

sliceBS :: Int -> Int -> BS.ByteString -> BS.ByteString
sliceBS from to xs = BS.take (to - from) (BS.drop from xs)


