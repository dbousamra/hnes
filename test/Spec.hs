import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Emulator               (step)
import           Emulator.Cartridge
import           Emulator.Monad
import           Emulator.Nes
import           Emulator.Trace         (Trace (..), renderTrace)
import           Emulator.Util          (prettifyWord16)
import           Nestest.Parsing        (parseTrace)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec            (parse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [nestest]

nestest :: TestTree
nestest = testCase "nestest" $ do
  rom <- BS.readFile "roms/cpu/nestest.nes"
  lines <- lines <$> readFile "roms/cpu/nestest.log"
  runIOEmulator rom $ do
    store (Cpu Pc) 0xC000
    emulate lines
  where
    emulate :: [String] -> IOEmulator ()
    emulate lines = do
      trace <- step
      case lines of
        [] -> pure ()
        (x:xs) -> case parse parseTrace "nestest.log" x of
          Left e -> liftIO $ assertFailure $ "Failed to parse " ++ show e
          Right nestestTrace -> do
            liftIO $ assertEqual ("Execution at " ++ (prettifyWord16 $ pc trace)) nestestTrace trace
            emulate xs
