module Emulator () where

import           Cartridge
import           Control.Monad.IO.Class
import           Data.ByteString        as BS hiding (putStrLn, replicate, take,
                                               zip)
import           Data.Word
import           Monad
import           Nes                    (Address (..))
import           Opcode
import           Util


readRom :: FilePath -> IO ByteString
readRom = BS.readFile

loadRom :: MonadEmulator m => BS.ByteString -> m ()
loadRom rom = loop 0 where
  len = BS.length rom
  loop i
    | i + 1 >= len = pure ()
    | otherwise    = do
      let byte = BS.index rom i
      let addr = fromIntegral $ i
      store (Ram8 addr) byte
      loop (i + 1)

loadNextOpcode :: MonadEmulator m => m Opcode
loadNextOpcode = do
  pc <- load Pc
  pcv <- load (Ram8 pc)
  pure $ decodeOpcode pcv

execute :: (MonadIO m, MonadEmulator m) => Opcode -> m ()
execute opcode = do
  liftIO $ putStrLn (show opcode)
  pure ()

incrementPc :: MonadEmulator m => m ()
incrementPc = do
  pc <- load Pc
  store Pc (pc + 1)

emulate :: (MonadIO m, MonadEmulator m) => m ()
emulate = do
  opcode <- loadNextOpcode
  incrementPc
  execute opcode
  -- emulate

run :: FilePath -> IO ()
run fp = do
  rom <- readRom fp
  let cart = parseCartridge rom
  let prgBytes = take 10 (BS.unpack $ prgRom cart)
  putStrLn $ show $ fmap prettifyWord8 prgBytes
  runIOEmulator cart $ do
    -- store Pc 0xC000
    x <- load $ Ram8 0xC000
    liftIO $ putStrLn (show x)
    emulate

r :: IO ()
r = run "roms/nestest.nes"

run1942 :: IO ()
run1942 = run "roms/1942.nes"
