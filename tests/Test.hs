module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Common

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [unitTests, propertyBasedTests]

unitTests :: TestTree
unitTests = testGroup "unit tests" []

propertyBasedTests :: TestTree
propertyBasedTests =
  testGroup
    "property based tests"
    [ testProperty "length of splits" lengthOfSplitsProperty
    , testProperty "elements of splits" elementsOfSplitsProperty
    ]

lengthOfSplitsProperty :: [Int] -> Property
lengthOfSplitsProperty xs = length xs === length (splits xs)

elementsOfSplitsProperty :: [Int] -> Property
elementsOfSplitsProperty xs = xs === map (\(_, x, _) -> x) (splits xs)
