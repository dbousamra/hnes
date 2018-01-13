module Main where

import           Blargg.Spec      as Blargg
import           Nestest.Spec     as Nestest
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
    Nestest.test
  , Blargg.test
  ]

