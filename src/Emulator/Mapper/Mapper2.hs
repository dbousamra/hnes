{-# LANGUAGE RecordWildCards #-}

module Emulator.Mapper.Mapper2 (
    Mapper2(..)
  , new
  , read
  , write
) where

import           Data.IORef
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Cartridge          as Cartridge
import           Emulator.Util
import           Prelude                     hiding (read)

data Mapper2 = Mapper2
  { cart     :: Cartridge
  , prgBanks :: Int
  , prgBank1 :: IORef Int
  , prgBank2 :: IORef Int
  }

new :: Cartridge -> IO Mapper2
new cart @ Cartridge{..} = do
  let prgBanks = VUM.length (Cartridge.prgRom cart) `div` 0x4000
  prgBank1 <- newIORef 0
  prgBank2 <- newIORef $ prgBanks - 1
  pure $ Mapper2 cart prgBanks prgBank1 prgBank2

read :: Mapper2 -> Word16 -> IO Word8
read (Mapper2 Cartridge {..} _ prgBank1 prgBank2) addr
  | addr' <  0x2000 = VUM.unsafeRead chrRom addr'
  | addr' >= 0xC000 = do
    prgBank2V <- readIORef prgBank2
    VUM.unsafeRead prgRom ((prgBank2V * 0x4000) + (addr' - 0xC000))
  | addr' >= 0x8000 = do
    prgBank1V <- readIORef prgBank1
    VUM.unsafeRead prgRom ((prgBank1V * 0x4000) + (addr' - 0x8000))
  | addr' >= 0x6000 = VUM.unsafeRead sram (addr' - 0x6000)
  | otherwise = error $ "Erroneous cart read detected!: " ++ prettifyWord16 addr
  where addr' = fromIntegral addr

write :: Mapper2 -> Word16 -> Word8 -> IO ()
write (Mapper2 Cartridge {..} prgBanks prgBank1 _) addr v
  | addr' < 0x2000 = VUM.unsafeWrite chrRom addr' v
  | addr' >= 0x8000 = modifyIORef prgBank1 (const $ toInt v `mod` prgBanks)
  | addr' >= 0x6000 = VUM.unsafeWrite sram (addr' - 0x6000) v
  | otherwise = error $ "Erroneous cart write detected!" ++ prettifyWord16 addr
  where addr' = fromIntegral addr
