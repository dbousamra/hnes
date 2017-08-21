module Emulator () where

import           Control.Monad.IO.Class
import           Data.ByteString.Lazy   as BS hiding (putStrLn, replicate, zip)
import           Data.Word
import           Monad
import           Nes                    (Address (..))
import           Opcode

readRom :: FilePath -> IO ByteString
readRom = BS.readFile

loadRom :: MonadEmulator m => ByteString -> m ()
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
run fp = runIOEmulator $ do
  rom <- liftIO $ readRom fp
  loadedRom <- loadRom rom
  resetVector <- load $ Ram16 0xFFFC
  store Pc resetVector
  emulate

runExample :: IO ()
runExample = run "roms/Example.nes"

run1942 :: IO ()
run1942 = run "roms/1942.nes"
