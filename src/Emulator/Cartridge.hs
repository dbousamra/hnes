module Emulator.Cartridge (
    Cartridge(..)
  , Mirror(..)
  , parse
) where

import           Control.Monad.ST
import           Data.Bits
import qualified Data.ByteString             as BS
import           Data.IORef
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Util               (sliceBS)
import           Prelude                     hiding (read)

data INesFileHeader = INesFileHeader {
  format   :: Word8,
  numPrg   :: Int,
  numChr   :: Int,
  control1 :: Int,
  control2 :: Int,
  numRam   :: Int
} deriving (Eq, Show)

data Mirror
  = MirrorHorizontal
  | MirrorVertical
  | MirrorSingle0
  | MirrorSingle1
  | MirrorFour
  deriving (Eq, Show, Enum)

data Cartridge = Cartridge {
  chrRom     :: VUM.MVector RealWorld Word8,
  prgRom     :: VUM.MVector RealWorld Word8,
  sram       :: VUM.MVector RealWorld Word8,
  mirror     :: IORef Mirror,
  mapperType :: Int
}

parseHeader :: BS.ByteString -> INesFileHeader
parseHeader bs = INesFileHeader
  (fromIntegral $ BS.index bs 3)
  (fromIntegral $ BS.index bs 4)
  (fromIntegral $ BS.index bs 5)
  (fromIntegral $ BS.index bs 6)
  (fromIntegral $ BS.index bs 7)
  (fromIntegral $ BS.index bs 8)

parse :: BS.ByteString -> IO Cartridge
parse bs = do
  let (INesFileHeader _ numPrg numChr ctrl1 ctrl2 _) = parseHeader bs
  let prgOffset = numPrg * prgRomSize
  let prgRom = sliceBS headerSize (headerSize + prgOffset) bs
  let chrOffset = numChr * chrRomSize
  let chrRom = if numChr == 0 then (BS.replicate chrRomSize 0)
               else sliceBS (headerSize + prgOffset) (headerSize + prgOffset + chrOffset) bs

  chr <- VU.unsafeThaw $ VU.fromList $ BS.unpack chrRom
  prg <- VU.unsafeThaw $ VU.fromList $ BS.unpack prgRom
  sram <- VUM.replicate 0x2000 0

  let mirrorV = (ctrl1 .&. 1) .|. (((ctrl1 `unsafeShiftR` 3) .&. 1) `unsafeShiftR` 1)
  mirror <- newIORef $ toEnum mirrorV

  let mapper = (ctrl1 `unsafeShiftR` 4) .|. ((ctrl2 `unsafeShiftR` 4) `unsafeShiftL` 4)

  pure $ Cartridge chr prg sram mirror mapper

headerSize :: Int
headerSize = 0x10

prgRomSize :: Int
prgRomSize = 0x4000

chrRomSize :: Int
chrRomSize = 0x2000
