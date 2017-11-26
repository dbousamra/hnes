module Blargg.Spec where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec      (parse)



test :: TestTree
test = testGroup "Tests" [
    testOne
  , testTwo
  ]

testOne :: TestTree
testOne = testCase "one" $
  assertBool "Equals" True

testTwo :: TestTree
testTwo = testCase "two" $
  assertBool "Equals" True
