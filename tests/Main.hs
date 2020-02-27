{-# LANGUAGE CPP #-}
module Main ( main ) where

import qualified Test.Tasty as T


#if defined(ARCH_ELF)
import Assemble ( assembleTests )
import Binaries ( binaryTests )
import Roundtrip ( roundtripTests )

elfCases :: [T.TestTree]
elfCases =
  [ assembleTests
  , binaryTests
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
  elfCases
