{-# LANGUAGE DuplicateRecordFields #-}

module Emulator.Cartridge (
    Cartridge(..)
  , parseCart
  , readCart
  , writeCart
) where

import           Control.Monad               (forM_)
import           Control.Monad.ST
import           Data.Bits                   (shiftL, unsafeShiftR, (.&.),
                                              (.|.))
import qualified Data.ByteString             as BS
import           Data.STRef
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Debug.Trace
import           Emulator.Util               (prettifyWord16, sliceBS, toInt)
import           Prelude                     hiding (read)

data INesFileHeader = INesFileHeader {
  numPrg   :: Int,
  numChr   :: Int,
  control1 :: Int,
  control2 :: Int,
  numRam   :: Int
} deriving (Eq, Show)

data ParsedCartridge =
  ParsedCartridge
    Int -- mirror
    [Word8] --chr,
    [Word8] --prg
    deriving (Show, Eq)

data Cartridge s = Cartridge {
  mirror :: Int,
  chr    :: VUM.MVector s Word8,
  prg    :: VUM.MVector s Word8,
  sRam   :: VUM.MVector s Word8
}

parseHeader :: BS.ByteString -> INesFileHeader
parseHeader bs = INesFileHeader
  (fromIntegral $ BS.index bs 4)
  (fromIntegral $ BS.index bs 5)
  (fromIntegral $ BS.index bs 6)
  (fromIntegral $ BS.index bs 7)
  (fromIntegral $ BS.index bs 8)

parseCart' :: BS.ByteString -> ParsedCartridge
parseCart' rom =
  let header @ (INesFileHeader numPrg numChr control1 control2 _) = parseHeader rom
      mirror    = (control1 .&. 1) .|. (shiftL ((unsafeShiftR control1 3) .&. 1) 1)
      prgLength = numPrg * prgSize
      chrLength = numChr * chrSize
      chr = if numChr == 0 then BS.unpack $ BS.replicate chrSize 0
            else BS.unpack $ sliceBS (headerSize + prgLength) (headerSize + prgLength + chrLength) rom
      prg = BS.unpack $ sliceBS headerSize prgLength rom

  in ParsedCartridge mirror chr prg

parseCart :: BS.ByteString -> ST s (Cartridge s)
parseCart rom = do
  let (ParsedCartridge mirror chr prg) = parseCart' rom
  chrRom <- VU.unsafeThaw $ VU.fromList chr
  prgRom <- VU.unsafeThaw $ VU.fromList prg
  sram <- VUM.replicate 0x2000 0
  pure $ Cartridge mirror chrRom prgRom sram

readCart :: Cartridge s -> Word16 -> ST s Word8
readCart cart addr
  | addr' <  0x2000 = VUM.read (chr cart) addr'
  | addr' >= 0x8000 = VUM.read (prg cart) ((addr' - 0x8000) `mod` VUM.length (prg cart))
  | addr' >= 0x6000 = VUM.read (sRam cart) (addr' - 0x6000)
  | otherwise = error $ "Erroneous mapper2 read detected!: " ++ prettifyWord16 addr
  where addr' = fromIntegral addr

writeCart :: Cartridge s -> Word16 -> Word8 -> ST s ()
writeCart cart addr v
  | addr' < 0x2000 = VUM.write (chr cart) addr' v
  | addr' >= 0x8000 = pure ()
  | addr' >= 0x6000 = VUM.write (sRam cart) (addr' - 0x6000) v
  | otherwise = error $ "Erroneous mapper2 write detected!" ++ prettifyWord16 addr
  where addr' = fromIntegral addr

headerSize :: Int
headerSize = 0x10

trainerSize :: Int
trainerSize = 0x200

prgSize :: Int
prgSize = 0x4000

chrSize :: Int
chrSize = 0x2000
