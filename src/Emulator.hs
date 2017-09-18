module Emulator (
    run
  , runDebug
  , r
  , emulateDebug
  , step
  , reset
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits              hiding (bit)
import qualified Data.ByteString        as BS
import           Data.Word
import           Emulator.Address
import           Emulator.Cartridge
import qualified Emulator.CPU           as CPU
import           Emulator.Monad
import           Emulator.Opcode
import qualified Emulator.PPU           as PPU
import           Emulator.Trace         (Trace (..), renderTrace)
import           Emulator.Util
import           Prelude                hiding (and, compare)
import           Text.Printf            (printf)

r :: IO ()
r = void $ runDebug "roms/color_test.nes" Nothing

run :: FilePath -> IO ()
run fp = void $ runDebug fp Nothing

runDebug :: FilePath -> Maybe Word16 -> IO [Trace]
runDebug fp startPc = do
  cart <- parseCartridge <$> BS.readFile fp
  runIOEmulator cart $ do
    case startPc of
      Just v  -> store (CpuAddress Pc) v
      Nothing -> reset
    emulateDebug 1000000000

emulate :: (MonadIO m, MonadEmulator m) => m ()
emulate = step >> emulate

emulateDebug :: (MonadIO m, MonadEmulator m) => Int -> m [Trace]
emulateDebug n = go 0 n [] where
  go c n acc = do
    trace <- step
    liftIO $ putStrLn $ renderTrace trace
    if c > n then pure acc
    else go (c + 1) n (acc ++ [trace])

step :: (MonadIO m, MonadEmulator m) => m Trace
step = do
  (cycles, trace) <- CPU.step
  replicateM_ (cycles * 3) PPU.step
  pure trace

reset :: MonadEmulator m => m ()
reset = CPU.reset >> PPU.reset

