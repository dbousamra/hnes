module Emulator (
    run
  , runDebug
  , r
  , step
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits              hiding (bit)
import qualified Data.ByteString        as BS
import           Data.Word
import           Emulator.Address
import           Emulator.Cartridge
import qualified Emulator.CPU.Execution as CPU
import           Emulator.Monad
import           Emulator.Opcode
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
    emulateDebug

emulate :: MonadEmulator m => m ()
emulate = step >> emulate

emulateDebug :: (MonadIO m, MonadEmulator m) => m [Trace]
emulateDebug = go [] where
  go acc = do
    trace <- step
    liftIO $ putStrLn $ renderTrace trace
    go (acc ++ [trace])

step :: MonadEmulator m => m Trace
step = CPU.step

reset :: MonadEmulator m => m ()
reset = CPU.reset

