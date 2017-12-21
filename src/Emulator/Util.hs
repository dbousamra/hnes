module Emulator.Util (
    prettifyWord16
  , prettifyWord8
  , makeW16
  , toWord8
  , toWord16
  , toInt
  , firstNibble
  , splitW16
  , sliceBS
  , catMaybesV
) where

import           Data.Bits       (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import           Data.Maybe      (fromJust, isJust)
import           Data.Vector     as V
import           Data.Word       (Word16, Word8)
import           Text.Printf     (printf)

prettifyWord16 :: Word16 -> String
prettifyWord16 = printf "%04X"

prettifyWord8 :: Word8 -> String
prettifyWord8 = printf "%02X"

makeW16 :: Word8 -> Word8 -> Word16
makeW16 lo hi = (toWord16 lo) .|. (toWord16 hi) `shiftL` 8

toWord8 :: Word16 -> Word8
toWord8 = fromIntegral

toWord16 :: Word8 -> Word16
toWord16 = fromIntegral

toInt :: Word8 -> Int
toInt = fromIntegral

splitW16 :: Word16 -> (Word8, Word8)
splitW16 w = (fromIntegral (w .&. 0xFF), fromIntegral (w `shiftR` 8))

firstNibble :: Word16 -> Word16
firstNibble = toWord16 . fst . splitW16

sliceBS :: Int -> Int -> BS.ByteString -> BS.ByteString
sliceBS from to xs = BS.take (to - from) (BS.drop from xs)

catMaybesV :: Vector (Maybe a) -> Vector a
catMaybesV = (V.map fromJust) . (V.filter isJust)


