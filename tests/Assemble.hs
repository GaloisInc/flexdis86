-- | Test that assembling a string using GCC produces the same binary
-- as assembling the output of Flexdis86's 'D.mkInstruction' using
-- Flexdis86's assembler.
module Assemble ( assembleTests ) where

import qualified Control.Lens as L
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as B
import           Data.Maybe ( mapMaybe )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import           Text.Show.Pretty ( ppShow )

import qualified Flexdis86 as D
import           Flexdis86.Prefixes ( prOSO )

import           Hexdump
import           Util ( AsmFlavor(..), withAssembledCode )

assembleTests :: T.TestTree
assembleTests =
  T.testGroup "Assemble Tests" (map mkTest testCases)

j20 :: [D.Value]
j20 = [D.JumpOffset D.JSize8 (D.FixedOffset (20 - 2))]

-- Since 'D.mkInstruction' uses Intel syntax, the tests here that use
-- AT&T syntax for the literal ASM may use different opcodes and will
-- have the arguments in the opposite order in the corresponding
-- 'D.mkInstruction' call.
testCases :: [(AsmFlavor, String, Maybe D.InstructionInstance)]
testCases = [ (Att, "ret", mkI "ret" [])
            , (Att, "int $0x3", mkI "int3" [])
            , (Att, "push $0x8", mkI "push" [D.ByteImm 8])
            , (Att, "pushw $0xfff", fmap setOSO $ mkI "push" [D.WordImm 0xfff])
            , (Att, "push $0x2000000", mkI "push" [D.DWordImm (D.Imm32Concrete 0x2000000)])
              -- The subtraction here is gross, but required because
              -- the jump is relative to the IP, which is incremented
              -- past the jump by the time it executes.
              --
              -- This only affects us because the assembler would
              -- normally do the rewriting automatically (so we have
              -- to as well)
            , (Att, "jmp .+20", mkI "jmp" j20)
            , (Att, "jmp .+2000", mkI "jmp" [D.JumpOffset D.JSize32 (D.FixedOffset (2000 - 5))])

            -- Warning (64-bit) xors here have multiple encodings --
            -- e.g. @xor %rdx, %rdx@ can be encoded as both 0x4831d2
            -- and 0x4833d2 -- and so these tests depend on flexdis
            -- choosing the same encoding as gcc.
            , (Intel, "xor rdx, rdx", mkI "xor" [D.QWordReg D.RDX, D.QWordReg D.RDX])
            , (Intel, "xor rbx, rbx", mkI "xor" [D.QWordReg D.RBX, D.QWordReg D.RBX])
            , (Intel, "xor rcx, rcx", mkI "xor" [D.QWordReg D.RCX, D.QWordReg D.RCX])
            , (Intel, "xor r8, r8", mkI "xor" [D.QWordReg D.R8, D.QWordReg D.R8])
            , (Intel, "xor eax, ebx", mkI "xor" [D.DWordReg D.EAX, D.DWordReg D.EBX])
            , (Att
              , "movq $0x190000000,%r11"
              , mkI "mov" [D.QWordReg D.R11, D.QWordImm (D.UImm64Concrete 0x190000000)])
            , (Att
              , "movq $0x190000000,%rbx"
              , mkI "mov" [D.QWordReg D.RBX, D.QWordImm (D.UImm64Concrete 0x190000000)])
            -- For each of the next two tests, the mkInstruction
            -- output from one assembles to the GCC output of the
            -- other!
            , (Intel, "mov rax, r12", mkI "mov" [D.QWordReg D.RAX, D.QWordReg D.R12])
            , (Intel, "mov r8, rsp", mkI "mov" [D.QWordReg D.R8, D.QWordReg D.RSP])
            , (Att, "sub %rsp,(%rax)", mkI "sub" [D.Mem64 (D.Addr_64 D.DS (Just D.RAX) Nothing D.NoDisplacement), D.QWordReg D.RSP])
            , (Att, "sub (%rax),%rsp", mkI "sub" [D.QWordReg D.RSP, D.Mem64 (D.Addr_64 D.DS (Just D.RAX) Nothing D.NoDisplacement)])

            -- Instructions with mnemonic synonyms.
            , (Att, "jnb .+20", mkI "jnb" j20)
            , (Att, "jnb .+20", mkI "jae" j20)
            , (Att, "jnb .+20", mkI "jnc" j20)
            , (Att, "jae .+20", mkI "jnb" j20)
            , (Att, "jae .+20", mkI "jae" j20)
            , (Att, "jae .+20", mkI "jnc" j20)
            , (Att, "jnc .+20", mkI "jnb" j20)
            , (Att, "jnc .+20", mkI "jae" j20)
            , (Att, "jnc .+20", mkI "jnc" j20)
            ]

setOSO :: D.InstructionInstance -> D.InstructionInstance
setOSO ii = ii { D.iiPrefixes = L.over prOSO (const True) (D.iiPrefixes ii) }

mkI :: String -> [D.Value] -> Maybe D.InstructionInstance
mkI = D.mkInstruction

mkTest :: (AsmFlavor, String, Maybe D.InstructionInstance) -> T.TestTree
mkTest (_flavor, asm, Nothing) = T.testCase asm $ T.assertFailure ("The mkI failed for " ++ asm)
mkTest (flavor, asm, Just inst) = T.testCase asm $ do
  withAssembledCode flavor [asm] $ \codeBytes -> do
    let Just bldr = (D.assembleInstruction inst)
        sbs = LB.toStrict (B.toLazyByteString bldr)
        disAddrs = D.disassembleBuffer codeBytes
        disInsts = mapMaybe D.disInstruction disAddrs
        disString = case disInsts of
          [i] -> ppShow i
          _ -> "(disassembly failed)"
        msg = unlines [ "Assembled bytes"
                      , "Expected"
                      , prettyHex codeBytes
                      , "but got"
                      , prettyHex sbs
                      , "Expected"
                      , disString
                      , "but got"
                      , ppShow inst
                      ]
    T.assertEqual msg codeBytes sbs
