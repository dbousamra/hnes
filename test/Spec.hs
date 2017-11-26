module Main where

import qualified Blargg.Spec      as Blargg
import qualified Nestest.Spec     as Nestest
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "blargg" [
    Nestest.test
  , Blargg.test
  ]


