module SpecHelper (
    run
) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Data.Word
import           Emulator
import           Emulator.Nes
import           Test.Tasty.HUnit

run :: FilePath -> Emulator Word8 -> Word8 -> IO ()
run filename readResult expected = do
  rom  <- BS.readFile filename
  runEmulator rom $ do
    reset
    replicateM_ 300 stepFrame
    result <- readResult
    liftIO $ assertEqual "Return code" expected result
