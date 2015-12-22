module Main ( main ) where

import qualified Test.Tasty as T

import Roundtrip ( roundtripTests )

main :: IO ()
main = T.defaultMain testCases

testCases :: T.TestTree
testCases = T.testGroup "FlexdisTests" [
  roundtripTests
  ]
