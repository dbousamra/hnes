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
      mapperType = (control2 .&. 0xF0) .|. (unsafeShiftR control1 4)
      mirror     = (control1 .&. 1) .|. (shiftL ((unsafeShiftR control1 3) .&. 1) 1)
      prgOffset  = numPrg * prgRomSize
      chrOffset  = numChr * chrRomSize
      chrRom     = if numChr == 0 then (BS.replicate chrRomSize 0)
                   else sliceBS (headerSize + prgOffset) (headerSize + prgOffset + chrOffset) bs
      prgRom     = sliceBS headerSize (headerSize + prgOffset) bs
  chr <- VU.unsafeThaw $ VU.fromList $ BS.unpack chrRom
  prg <- VU.unsafeThaw $ VU.fromList $ BS.unpack prgRom
  sram <- VUM.replicate 0x2000 0
  prgBanks <- newSTRef $ VUM.length prg `div` 0x4000
  prgBank1 <- newSTRef $ 0
  prgBank2 <- newSTRef $ 0 - 1
  pure $ Cartridge header mirror prgBanks prgBank1 prgBank2 chr prg sram


readCart :: Cartridge s -> Word16 -> ST s Word8
readCart cart addr
  | addr' <  0x2000 = VUM.read (chrRom cart) addr'
  | addr' >= 0xC000 = VUM.read (prgRom cart) ((prgBank2 * 0x4000) + (addr' - 0xC000))
  | addr' >= 0x8000 = VUM.read (prgRom cart) ((prgBank1 * 0x4000) + (addr' - 0x8000))
  | addr' >= 0x6000 = VUM.read (prgRom cart) (addr' - 0x6000)
  | otherwise = error $ "Erroneous mapper2 read detected!: " ++ prettifyWord16 addr
  where
    addr' = fromIntegral addr
    prgBanks = VUM.length (prgRom cart) `div` 0x4000
    prgBank1 = 0
    prgBank2 = prgBanks - 1

writeCart :: Cartridge s -> Word16 -> Word8 -> ST s ()
writeCart cart addr v
  | addr' < 0x2000 = VUM.write (chrRom cart) addr' v
  | addr' >= 0x8000 = do
    prgBanks <- readSTRef (prgBanks cart)
    modifySTRef (prgBank1 cart) (const $ toInt v `mod` prgBanks)
  | addr' >= 0x6000 = VUM.write (sRam cart) (addr' - 0x6000) v
  | otherwise = error $ "Erroneous mapper2 write detected!" ++ prettifyWord16 addr
  where
    addr' = fromIntegral addr

headerSize :: Int
headerSize = 0x10

trainerSize :: Int
trainerSize = 0x200

prgRomSize :: Int
prgRomSize = 0x4000

chrRomSize :: Int
chrRomSize = 0x2000
