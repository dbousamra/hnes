module Emulator (
    run
  , runDebug
  , r
  , emulateDebug
  , step
  , stepFrame
  , reset
) where

import           Control.Monad
import           Control.Monad.Loops
import qualified Data.ByteString     as BS
import           Data.Word
import qualified Emulator.CPU        as CPU
import           Emulator.Monad
import           Emulator.Nes
import qualified Emulator.PPU        as PPU
import           Emulator.Trace      (Trace (..))
import           Prelude             hiding (and, compare)

r :: IO ()
r = void $ runDebug "roms/nestest.nes" Nothing

run :: FilePath -> IO ()
run fp = void $ runDebug fp Nothing

runDebug :: FilePath -> Maybe Word16 -> IO [Trace]
runDebug fp startPc = do
  bytes <-  BS.readFile fp
  runIOEmulator bytes $ do
    case startPc of
      Just v  -> store (Cpu Pc) v
      Nothing -> reset
    emulateDebug 1000000000

emulateDebug :: Int -> IOEmulator [Trace]
emulateDebug n = go 0 n [] where
  go c n' acc = do
    trace <- step
    if c > n' then pure acc
    else go (c + 1) n (acc ++ [trace])

step :: IOEmulator Trace
step = do
  (cycles', trace) <- CPU.step
  replicateM_ (cycles' * 3) PPU.step
  pure trace

stepFrame :: IOEmulator [Trace]
stepFrame = do
  frameCount <- load $ Ppu FrameCount
  untilM step $ do
    frameCount' <- load $ Ppu FrameCount
    pure $ frameCount' == (frameCount + 1)

reset :: IOEmulator ()
reset = CPU.reset >> PPU.reset

