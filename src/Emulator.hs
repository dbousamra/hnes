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

readBytes :: FilePath -> IO ByteString
readBytes = BS.readFile

loadNextOpcode :: (MonadIO m, MonadEmulator m) => m Opcode
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

emulate :: (MonadIO m, MonadEmulator m) => Int -> Int -> m ()
emulate n max =
  if n >= max then pure ()
  else do
    opcode <- loadNextOpcode
    execute opcode
    incrementPc
    emulate (n + 1) max

run :: FilePath -> IO ()
run fp = do
  cart <- parseCartridge <$> readBytes fp
  runIOEmulator cart $ do
    store Pc 0xC000
    emulate 0 10

r :: IO ()
r = run "roms/nestest.nes"

run1942 :: IO ()
run1942 = run "roms/1942.nes"
