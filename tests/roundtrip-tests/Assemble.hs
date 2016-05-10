module Assemble ( assembleTests ) where

import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Flexdis86 as D
import qualified Flexdis86.Register as D
import Flexdis86.Prefixes ( prOSO )
import Hexdump

import Util ( withAssembledCode )

assembleTests :: T.TestTree
assembleTests =
  T.testGroup "Assemble Tests" (map mkTest testCases)

testCases :: [(String, Maybe D.InstructionInstance)]
testCases = [ ("ret", mkI "ret" [])
            , ("int $0x3", mkI "int3" [])
            , ("push $0x8", mkI "push" [D.ByteImm 8])
            , ("pushw $0xfff", fmap setOSO $ mkI "push" [D.WordImm 0xfff])
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

            -- Warning (64-bit) xors here have multiple encodings --
            -- e.g. @xor %rdx, %rdx@ can be encoded as both 0x4831d2
            -- and 0x4833d2 -- and so these tests depend on flexdis
            -- choosing the same encoding as gcc.
            , ("xor %rdx, %rdx", mkI "xor" [D.QWordReg D.rdx, D.QWordReg D.rdx])
            , ("xor %rbx, %rbx", mkI "xor" [D.QWordReg D.rbx, D.QWordReg D.rbx])
            , ("xor %rcx, %rcx", mkI "xor" [D.QWordReg D.rcx, D.QWordReg D.rcx])
            , ("xor %r8, %r8", mkI "xor" [D.QWordReg (D.reg64 8), D.QWordReg (D.reg64 8)])
            , ("movq $0x190000000,%r11", mkI "mov" [D.QWordReg (D.reg64 11), D.QWordImm 0x190000000])

            -- Instructions with mnemonic synonyms.
            , ("jnb .+20", mkI "jnb" [D.JumpOffset D.BSize (20 - 2)])
            , ("jnb .+20", mkI "jae" [D.JumpOffset D.BSize (20 - 2)])
            , ("jnb .+20", mkI "jnc" [D.JumpOffset D.BSize (20 - 2)])
            , ("jae .+20", mkI "jnb" [D.JumpOffset D.BSize (20 - 2)])
            , ("jae .+20", mkI "jae" [D.JumpOffset D.BSize (20 - 2)])
            , ("jae .+20", mkI "jnc" [D.JumpOffset D.BSize (20 - 2)])
            , ("jnc .+20", mkI "jnb" [D.JumpOffset D.BSize (20 - 2)])
            , ("jnc .+20", mkI "jae" [D.JumpOffset D.BSize (20 - 2)])
            , ("jnc .+20", mkI "jnc" [D.JumpOffset D.BSize (20 - 2)])
            ]

setOSO :: D.InstructionInstance -> D.InstructionInstance
setOSO ii = ii { D.iiPrefixes = L.over prOSO (const True) (D.iiPrefixes ii) }

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
