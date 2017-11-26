module Main where

import qualified Nestest.Spec     as Nestest
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Nestest.test]


