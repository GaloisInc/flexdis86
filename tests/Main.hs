{-# LANGUAGE CPP #-}
module Main ( main ) where

import qualified Test.Tasty as T

import Binaries ( binaryTests )

#if defined(ARCH_ELF)
import Assemble ( assembleTests )
import Roundtrip ( roundtripTests )

elfCases :: [T.TestTree]
elfCases =
  [ assembleTests
  , roundtripTests
  ]
#else

elfCases :: [T.TestTree]
elfCases = []

#endif

main :: IO ()
main = T.defaultMain testCases

testCases :: T.TestTree
testCases = T.testGroup "FlexdisTests" $
  binaryTests : elfCases
