import qualified Data.ByteString.Lazy.Char8 as BS
import           Test.Tasty
import           Test.Tasty.Golden

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [nestest]

nestest :: TestTree
nestest = goldenVsString "nestest" "roms/nestest.log" action
  where
    action :: IO BS.ByteString
    action = pure $ BS.pack []

