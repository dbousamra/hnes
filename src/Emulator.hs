module Emulator () where

import           Control.Monad.IO.Class
import           Data.ByteString.Lazy   as BS hiding (putStrLn, replicate, zip)
import           Data.Word
import           Monad
import           Nes                    (Address (..))

data Opcode
  = Opcode Word8
  deriving (Show)

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
      store (Address addr) byte
      loop (i + 1)

loadOpcode :: MonadEmulator m => m Opcode
loadOpcode = do
  pure undefined
  -- pc  <- load16 Pc
  -- pcv <- load8 (Ram pc)
  -- pure $ Opcode pcv

executeOpcode :: (MonadIO m, MonadEmulator m) => Opcode -> m ()
executeOpcode opcode = do
  liftIO $ putStrLn (show opcode)
  -- pc <- load16 Pc
  -- store16 Pc (pc + 1)
  pure undefined

emulate :: (MonadIO m, MonadEmulator m) => m ()
emulate = do
  opcode <- loadOpcode
  executeOpcode opcode
  liftIO $ putStrLn "In emulate"
  emulate

run :: FilePath -> IO ()
run fp = runIOEmulator $ do
  rom <- liftIO $ readRom fp
  loadedRom <- loadRom rom
  emulate

runExample :: IO ()
runExample = run "roms/Example.nes"

runDrMario :: IO ()
runDrMario = run "roms/Dr_Mario.nes"
