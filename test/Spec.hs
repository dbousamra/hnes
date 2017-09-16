import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Emulator               (step)
import           Emulator.Address
import           Emulator.Cartridge
import           Emulator.Monad
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
  cart <- parseCartridge <$> BS.readFile "roms/nestest.nes"
  lines <- lines <$> readFile "roms/nestest.log"
  runIOEmulator cart $ do
    store (CpuAddress Pc) 0xC000
    emulate lines
  where
    emulate :: (MonadEmulator m, MonadIO m) => [String] -> m ()
    emulate lines = do
      trace <- step
      case lines of
        [] -> pure ()
        (x:xs) -> case parse parseTrace "nestest.log" x of
          Left e -> liftIO $ assertFailure $ "Failed to parse " ++ show e
          Right nestestTrace -> do
            liftIO $ print trace
            liftIO $ assertEqual ("Execution at " ++ (prettifyWord16 $ pc trace)) nestestTrace trace
            emulate xs
