import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Emulator               (execute, loadNextOpcode)
import           Emulator.Cartridge
import           Emulator.Monad
import           Emulator.Nes           (Address (..))
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
nestest = testCase "" $ do
  cart <- parseCartridge <$> BS.readFile "roms/nestest.nes"
  lines <- lines <$> readFile "roms/nestest.log"
  runIOEmulator cart $ do
    store Pc 0xC000
    emulate lines
  where
    emulate :: (MonadEmulator m, MonadIO m) => [String] -> m ()
    emulate lines = do
      opcode <- loadNextOpcode
      trace <- execute opcode
      let parsed = parse parseTrace "nestest.log" (head lines)
      case parsed of
        Left e -> liftIO $ assertFailure "Failed to parse"
        Right nestestTrace -> do
          liftIO $ assertEqual ("Execution at " ++ (prettifyWord16 $ pc trace)) nestestTrace trace
          emulate $ tail lines
