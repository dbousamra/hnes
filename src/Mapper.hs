module Mapper (
  -- * Types
  -- * Functions
    parseMapper
) where

import qualified Data.ByteString as BS
import           Data.Word
import           Util            (sliceBS)

data INesFileHeader = INesFileHeader {
  format   :: Word8,
  numPrg   :: Int,
  numChr   :: Int,
  control1 :: Int,
  control2 :: Int,
  numRam   :: Int
} deriving (Eq, Show)

parseMapper :: BS.ByteString -> INesFileHeader
parseMapper bs = INesFileHeader
  (fromIntegral $ BS.index bs 4)
  (fromIntegral $ BS.index bs 5)
  (fromIntegral $ BS.index bs 6)
  (fromIntegral $ BS.index bs 7)
  (fromIntegral $ BS.index bs 8)
  (fromIntegral $ BS.index bs 9)
