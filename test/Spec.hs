module Main where

import           Blargg.Spec            as Blargg
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import           Emulator
import           Emulator.CPU           (stepT)
import           Emulator.Monad
import           Emulator.Nes
import           Emulator.Trace
import           Emulator.Util
import           Nestest.Spec           as Nestest
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
    Nestest.test
  , Blargg.test
  ]

