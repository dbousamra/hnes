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
import           Emulator.Util               (prettifyWord16, sliceBS, toInt)
import           Prelude                     hiding (read)

data INesFileHeader = INesFileHeader {
  format   :: Word8,
  numPrg   :: Int,
  numChr   :: Int,
  control1 :: Int,
  control2 :: Int,
  numRam   :: Int
} deriving (Eq, Show)

data Cartridge s = Cartridge {
  header :: INesFileHeader,
  mirror :: Int,
  chrRom :: VUM.MVector s Word8,
  prgRom :: VUM.MVector s Word8,
  sRam   :: VUM.MVector s Word8
}

parseHeader :: BS.ByteString -> INesFileHeader
parseHeader bs = INesFileHeader
  (fromIntegral $ BS.index bs 3)
  (fromIntegral $ BS.index bs 4)
  (fromIntegral $ BS.index bs 5)
  (fromIntegral $ BS.index bs 6)
  (fromIntegral $ BS.index bs 7)
  (fromIntegral $ BS.index bs 8)

parseCart :: BS.ByteString -> ST s (Cartridge s)
parseCart bs = do
  let header @ (INesFileHeader _ numPrg numChr control1 control2 _) = parseHeader bs
  let mirror = 1
  let prgOffset  = numPrg * prgRomSize
  let prgRom = sliceBS headerSize (headerSize + prgOffset) bs
  let chrOffset = numChr * chrRomSize
  let chrRom = if numChr == 0 then (BS.replicate chrRomSize 0)
               else sliceBS (headerSize + prgOffset) (headerSize + prgOffset + chrOffset) bs

  prg <- VU.unsafeThaw $ VU.fromList $ BS.unpack prgRom
  chr <- VU.unsafeThaw $ VU.fromList $ BS.unpack chrRom
  sram <- VUM.replicate 0x2000 0

  pure $ Cartridge header mirror chr prg sram

readCart :: Cartridge s -> Word16 -> ST s Word8
readCart cart addr
  | addr' <  0x2000 = VUM.read (chrRom cart) addr'
  | addr' >= 0x8000 = do
    let index = (addr' - 0x8000 ) `mod` VUM.length (prgRom cart)
    VUM.read (prgRom cart) index
  | addr' >= 0x6000 = VUM.read (prgRom cart) (addr' - 0x6000)
  | otherwise = error $ "Erroneous cart read detected!: " ++ prettifyWord16 addr
  where addr' = fromIntegral addr

writeCart :: Cartridge s -> Word16 -> Word8 -> ST s ()
writeCart cart addr v
  | addr' < 0x2000 = VUM.write (chrRom cart) addr' v
  | addr' >= 0x6000 = VUM.write (sRam cart) (addr' - 0x6000) v
  | otherwise = error $ "Erroneous cart write detected!" ++ prettifyWord16 addr
  where addr' = fromIntegral addr

headerSize :: Int
headerSize = 0x10

trainerSize :: Int
trainerSize = 0x200

prgRomSize :: Int
prgRomSize = 0x4000

chrRomSize :: Int
chrRomSize = 0x2000
