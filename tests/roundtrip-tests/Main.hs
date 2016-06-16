{-# LANGUAGE CPP #-}
module Main ( main ) where

import qualified Test.Tasty as T


#ifdef OS_LINUX
import Assemble ( assembleTests )
import Roundtrip ( roundtripTests )
#endif

main :: IO ()
main = T.defaultMain testCases

#ifdef OS_LINUX
-- | Test cases that depend on being able to build Elf files.
elfCases :: [T.TestTree]
elfCases =
  [ assembleTests
  , roundtripTests
  ]
#else
elfCases :: [T.TestTree]
elfCases = []
#endif


testCases :: T.TestTree
testCases = T.testGroup "FlexdisTests" $
  elfCases
