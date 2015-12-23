module Assemble ( assembleTests ) where

import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Flexdis86 as D
import Hexdump

import Util ( withAssembledCode )

assembleTests :: T.TestTree
assembleTests =
  T.testGroup "Assemble Tests" (map mkTest testCases)

testCases :: [(String, Maybe D.InstructionInstance)]
testCases = [ ("ret", mkI "ret" [])
            , ("int $0x3", mkI "int3" [])
            , ("push $0x8", mkI "push" [D.ByteImm 8])
            , ("push $0xfff", mkI "push" [D.WordImm 0xfff])
            , ("push $0x2000000", mkI "push" [D.DWordImm 0x2000000])
              -- The subtraction here is gross, but required because
              -- the jump is relative to the IP, which is incremented
              -- past the jump by the time it executes.
              --
              -- This only affects us because the assembler would
              -- normally do the rewriting automatically (so we have
              -- to as well)
            , ("jmp .+20", mkI "jmp" [D.JumpOffset D.BSize (20 - 2)])
            , ("jmp .+2000", mkI "jmp" [D.JumpOffset D.ZSize (2000 - 5)])
            ]

mkI :: String -> [D.Value] -> Maybe D.InstructionInstance
mkI = D.mkInstruction D.defaultX64Assembler

mkTest :: (String, Maybe D.InstructionInstance) -> T.TestTree
mkTest (asm, Nothing) = T.testCase asm $ T.assertFailure ("Could not assemble " ++ asm)
mkTest (asm, Just inst) = T.testCase asm $ do
  withAssembledCode [asm] $ \codeBytes -> do
    let Just bldr = (D.assembleInstruction inst)
        sbs = LB.toStrict (B.toLazyByteString bldr)
        msg = unlines [ "Assembled bytes"
                      , "Expected"
                      , prettyHex codeBytes
                      , "but got"
                      , prettyHex sbs
                      ]
    T.assertEqual msg codeBytes sbs
