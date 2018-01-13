module Nestest.Spec where

import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Emulator.CPU           (stepT)
import           Emulator.Nes
import qualified Emulator.Trace         as Trace
import           Emulator.Util          (prettifyWord16)
import           Nestest.Parsing        (parseTrace)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec            (parse)

test :: TestTree
test = testCase "nestest" $ do
  rom <- BS.readFile "roms/tests/cpu/nestest/nestest.nes"
  lines <- lines <$> readFile "roms/tests/cpu/nestest/nestest.log"
  runEmulator rom $ do
    storeCpu pc 0xC000
    emulate lines
  where
    emulate :: [String] -> Emulator ()
    emulate lines = do
      (_, trace) <- stepT
      case lines of
        [] -> pure ()
        (x:xs) -> case parse parseTrace "nestest.log" x of
          Left e -> liftIO $ assertFailure $ "Failed to parse " ++ show e
          Right nestestTrace -> do
            liftIO $ assertEqual ("Execution at " ++ prettifyWord16 (Trace.pc trace)) nestestTrace trace
            emulate xs
