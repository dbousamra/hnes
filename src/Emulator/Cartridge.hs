module Emulator.Cartridge (
    Cartridge(..)
  , Mapper(..)
  , parseCartridge
  , mapper0
) where

import           Data.Bits       (shiftL, unsafeShiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import           Data.Word
import           Emulator.Util   (prettifyWord16, sliceBS)

headerSize :: Int
headerSize = 0x10

trainerSize :: Int
trainerSize = 0x200

prgRomSize :: Int
prgRomSize = 0x4000

chrRomSize :: Int
chrRomSize = 0x2000

data INesFileHeader = INesFileHeader {
  format   :: Word8,
  numPrg   :: Int,
  numChr   :: Int,
  control1 :: Int,
  control2 :: Int,
  numRam   :: Int
} deriving (Eq, Show)

data Cartridge = Cartridge {
  header :: INesFileHeader,
  mirror :: Int,
  chrRom :: BS.ByteString,
  prgRom :: BS.ByteString,
  sRam   :: BS.ByteString
} deriving (Eq, Show)

data Mapper = Mapper {
  cart    :: Cartridge,
  readRom :: Word16 -> Word8
}

parseINesFileHeader :: BS.ByteString -> INesFileHeader
parseINesFileHeader bs = INesFileHeader
  (fromIntegral $ BS.index bs 3)
  (fromIntegral $ BS.index bs 4)
  (fromIntegral $ BS.index bs 5)
  (fromIntegral $ BS.index bs 6)
  (fromIntegral $ BS.index bs 7)
  (fromIntegral $ BS.index bs 8)

parseCartridge :: BS.ByteString -> Cartridge
parseCartridge bs =
  let header @ (INesFileHeader _ numPrg numChr control1 control2 _) = parseINesFileHeader bs
      mapperType = (control2 .&. 0xF0) .|. (unsafeShiftR control1 4)
      mirror     = (control1 .&. 1) .|. (shiftL ((unsafeShiftR control1 3) .&. 1) 1)
      prgOffset  = numPrg * prgRomSize
      chrOffset  = numChr * chrRomSize
      prgRom     = sliceBS headerSize (headerSize + prgOffset) bs
      chrRom     = if numChr == 0 then (BS.replicate chrRomSize 0)
                   else sliceBS (headerSize + prgOffset) (headerSize + prgOffset + chrOffset) bs
      sRam       = BS.replicate 0x2000 0
  in
    Cartridge header mirror chrRom prgRom sRam

mapper0 :: Cartridge -> Mapper
mapper0 cart = Mapper cart readRom where
  readRom r
    | addr <  0x2000 = BS.index (chrRom cart) addr
    | addr >= 0xC000 = BS.index (prgRom cart) ((prgBank2 * 0x4000) + (addr - 0xC000))
    | addr >= 0x8000 = BS.index (prgRom cart) ((prgBank1 * 0x4000) + (addr - 0x8000))
    | addr >= 0x6000 = BS.index (prgRom cart) (addr - 0x6000)
    | otherwise = error $ "Erroneous mapper0 read detected!: " ++ prettifyWord16 r
    where
      addr = fromIntegral r
      prgBanks = BS.length (prgRom cart) `div` 0x4000
      prgBank1 = 0
      prgBank2 = prgBanks - 1
