module Assemble ( assembleTests ) where

import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Flexdis86 as D

import Util ( withAssembledCode )

assembleTests :: T.TestTree
assembleTests =
  T.testGroup "Assemble Tests" (map mkTest testCases)

testCases :: [(String, Maybe D.InstructionInstance)]
testCases = [ ("ret", mkI "ret" [])
            , ("int $0x3", mkI "int3" [])
            ]

mkI :: String -> [D.Value] -> Maybe D.InstructionInstance
mkI = D.mkInstruction D.defaultX64Assembler

mkTest :: (String, Maybe D.InstructionInstance) -> T.TestTree
mkTest (asm, Nothing) = T.testCase asm $ T.assertFailure ("Could not assemble " ++ asm)
mkTest (asm, Just inst) = T.testCase asm $ do
  withAssembledCode [asm] $ \codeBytes -> do
    let Just bldr = (D.assembleInstruction inst)
        sbs = LB.toStrict (B.toLazyByteString bldr)
    T.assertEqual "Assembled bytes" codeBytes sbs
