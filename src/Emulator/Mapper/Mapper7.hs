{-# LANGUAGE RecordWildCards #-}

module Emulator.Mapper.Mapper7 (
    Mapper7(..)
  , new
  , read
  , write
) where

import           Data.IORef
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Data.Bits ((.&.))
import           Emulator.Cartridge          as Cartridge
import           Emulator.Util
import           Prelude                     hiding (read)

data Mapper7 = Mapper7
  { cart     :: Cartridge
  , prgBank  :: IORef Int
  }

new :: Cartridge -> IO Mapper7
new cart@Cartridge{..} = do
  prgBank <- newIORef 0
  pure $ Mapper7 cart prgBank

read :: Mapper7 -> Word16 -> IO Word8
read (Mapper7 Cartridge {..} prgBank) addr
  | addr' <  0x2000 = VUM.read chrRom addr'
  | addr' >= 0x8000 = do
    prgBankV <- readIORef prgBank
    VUM.read prgRom ((prgBankV * 0x8000) + (addr' - 0x8000))
  | addr' >= 0x6000 = VUM.read sram (addr' - 0x6000)
  | otherwise = error $ "Erroneous cart read detected!: " ++ prettifyWord16 addr
  where addr' = fromIntegral addr

write :: Mapper7 -> Word16 -> Word8 -> IO ()
write (Mapper7 Cartridge {..} prgBank) addr v
  | addr' < 0x2000 = VUM.write chrRom addr' v
  | addr' >= 0x8000 = do
    modifyIORef' prgBank (const $ fromIntegral v .&. 7)
    let m = case v .&. 0x10 of
          0x00 -> MirrorSingle0
          0x10 -> MirrorSingle1
    writeIORef mirror m
  | addr' >= 0x6000 = VUM.write sram (addr' - 0x6000) v
  | otherwise = error $ "Erroneous cart write detected!" ++ prettifyWord16 addr
  where addr' = fromIntegral addr


