module Emulator.Cartridge (
    Cartridge(..)
  , parse
  , read
  , write
) where

import           Control.Monad.ST
import qualified Data.ByteString             as BS
import           Data.IORef
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

data Cartridge = Cartridge {
  chrRom   :: VUM.MVector RealWorld Word8,
  prgRom   :: VUM.MVector RealWorld Word8,
  sRam     :: VUM.MVector RealWorld Word8,
  prgBanks :: Int,
  chrBanks :: Int,
  prgBank1 :: IORef Int,
  prgBank2 :: IORef Int,
  chrBank1 :: IORef Int
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
  let (INesFileHeader _ numPrg numChr _ _ _) = parseHeader bs
  let prgOffset = numPrg * prgRomSize
  let prgRom = sliceBS headerSize (headerSize + prgOffset) bs
  let chrOffset = numChr * chrRomSize
  let chrRom = if numChr == 0 then (BS.replicate chrRomSize 0)
               else sliceBS (headerSize + prgOffset) (headerSize + prgOffset + chrOffset) bs

  chr <- VU.unsafeThaw $ VU.fromList $ BS.unpack chrRom
  prg <- VU.unsafeThaw $ VU.fromList $ BS.unpack prgRom
  sram <- VUM.replicate 0x2000 0

  let prgBanks = VUM.length prg `div` 0x4000
  prgBank1 <- newIORef 0
  prgBank2 <- newIORef $ prgBanks - 1

  let chrBanks = VUM.length chr `div` 0x2000
  chrBank1 <- newIORef 0

  pure $ Cartridge chr prg sram prgBanks chrBanks prgBank1 prgBank2 chrBank1

read :: Cartridge -> Word16 -> IO Word8
read (Cartridge chr prg sram _ _ prgBank1 prgBank2 _) addr
  | addr' <  0x2000 = VUM.unsafeRead chr addr'
  | addr' >= 0xC000 = do
    prgBank2V <- readIORef prgBank2
    VUM.unsafeRead prg ((prgBank2V * 0x4000) + (addr' - 0xC000))
  | addr' >= 0x8000 = do
    prgBank1V <- readIORef prgBank1
    VUM.unsafeRead prg ((prgBank1V * 0x4000) + (addr' - 0x8000))
  | addr' >= 0x6000 = VUM.unsafeRead sram (addr' - 0x6000)
  | otherwise = error $ "Erroneous cart read detected!: " ++ prettifyWord16 addr
  where addr' = fromIntegral addr

write :: Cartridge -> Word16 -> Word8 -> IO ()
write (Cartridge chr _ sram _ _ prgBank1 _ _) addr v
  | addr' < 0x2000 = VUM.unsafeWrite chr addr' v
  | addr' >= 0x8000 = modifyIORef prgBank1 (const $ toInt v)
  | addr' >= 0x6000 = VUM.unsafeWrite sram (addr' - 0x6000) v
  | otherwise = error $ "Erroneous cart write detected!" ++ prettifyWord16 addr
  where addr' = fromIntegral addr

headerSize :: Int
headerSize = 0x10

prgRomSize :: Int
prgRomSize = 0x4000

chrRomSize :: Int
chrRomSize = 0x2000
