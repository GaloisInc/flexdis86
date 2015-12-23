module Roundtrip ( roundtripTests ) where

import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy as LB
import Data.Maybe ( mapMaybe )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Flexdis86 as D
import Hexdump

import Util ( withAssembledCode )

roundtripTests :: T.TestTree
roundtripTests = T.testGroup "Roundtrip Tests" [
  zeroOperandTests,
  immediateTests,
  singleOperandTests,
  twoOperandTests
  ]

zeroOperandTests :: T.TestTree
zeroOperandTests =
  T.testGroup "ZeroOperandOpcodes" (map mkTest zeroOperandOpcodeTests)

zeroOperandOpcodeTests :: [(String, [String])]
zeroOperandOpcodeTests = [ ("ret", ["ret"])
                         , ("x87 wait", ["wait"])
                         , ("int3", ["int $0x3"])
                           -- FIXME: This gets rendered as xor eax,eax
                           --
                           -- That is probably valid, but we can't
                           -- re-produce the 1-byte no

--                         , ("nop", ["nop"])
                         , ("halt", ["hlt"])
                         , ("cmc", ["cmc"])
                         , ("clear carry flag", ["clc"])
                         , ("set carry flag", ["stc"])
                         , ("clear interrupt flag", ["cli"])
                         , ("set interrupt flag", ["sti"])
                         , ("clear direction flag", ["cld"])
                         , ("set direction flag", ["std"])
                         , ("undefined instruction", ["ud2"])
                         , ("memory fence", ["mfence"])
                         , ("store fence", ["sfence"])
                         , ("load fence", ["lfence"])
                           -- This test (cwd) requires a size override
                           -- to 16 bits because it otherwise shares
                           -- an opcode with cdq
                         , ("convert word to dword", ["cwd"])
                         , ("convert dword to qword", ["cdq"])
                         , ("swapgs", ["swapgs"])
                         , ("xgetbv", ["xgetbv"])
                         , ("Empty MMX state", ["emms"])
                         ]

singleOperandTests :: T.TestTree
singleOperandTests =
  T.testGroup "SingleOperandOpcodes" (map mkTest singleOperandOpcodes)

singleOperandOpcodes :: [(String, [String])]
singleOperandOpcodes = [ ("increment r8/ah", ["inc %ah"])
                         -- %al is 0, while %ah is non-zero
                       , ("increment r8/al", ["inc %al"])
                       , ("increment r16/ax", ["inc %ax"])
                       , ("increment r16/bx", ["inc %bx"])
                       , ("increment r32", ["inc %eax"])
                       , ("increment r64", ["inc %rax"])
                         -- %edx is interesting because the encoding
                         -- of %eax is 0, while %edx is non-zero.
                         -- This will make sure we shift the REG field
                         -- correctly.
                       , ("increment edx", ["inc %edx"])
                         -- This one uses a memory reference through a register
                       , ("increment mem32 via eax", ["incl (%eax)"])
                       , ("increment mem32 via ecx", ["incl (%ecx)"])
                       , ("increment mem32 via esi", ["incl (%esi)"])
                       , ("increment mem32 via edi", ["incl (%edi)"])
                       , ("increment mem16 via eax", ["incw (%eax)"])
                       , ("increment mem16 via edx", ["incw (%edx)"])
                       , ("increment mem8 via eax", ["incb (%eax)"])
                       , ("increment mem8 via ebx", ["incb (%ebx)"])
                       , ("increment mem64 via eax", ["incq (%eax)"])
                       , ("increment mem64 via ecx", ["incq (%ecx)"])
                         -- Memory reference through a 64 bit register
                       , ("increment mem32 via rax", ["incl (%rax)"])
                       , ("increment mem32 via rdx", ["incl (%rdx)"])
                       , ("increment mem8 via r12", ["incb (%r12)"])
                       , ("increment mem8 via r13", ["incb (%r13)"])
                         -- Reg+disp8
                       , ("increment mem32 via edx+disp8", ["incl 0x10(%edx)"])
                       , ("increment mem32 via edx+disp32", ["incl 0x1000000(%edx)"])
                       , ("increment mem32 via rdx+disp8", ["incl 0x10(%rdx)"])
                       , ("increment mem32 via rdx+disp32", ["incl 0x1000000(%rdx)"])
                       ]

twoOperandTests :: T.TestTree
twoOperandTests =
  T.testGroup "TwoOperandTests" (map mkTest twoOperandOpcodes)

twoOperandOpcodes :: [(String, [String])]
twoOperandOpcodes = [ ("test reg reg (eax)", ["test %eax, %eax"])
                    , ("test reg reg (edx)", ["test %edx, %edx"])
                    , ("test reg reg (ebx, ecx)", ["test %ebx, %ecx"])
                    , ("mov r8, r8", ["mov %al, %bl"])
                    , ("mov r8, imm8", ["mov $8, %bl"])
                    , ("mov r64, imm64", ["mov $10000000000, %r9"])
                    ]

immediateTests :: T.TestTree
immediateTests =
  T.testGroup "ImmediateOperandOpcodes" (map mkTest immediateOperandOpcodes)

immediateOperandOpcodes :: [(String, [String])]
immediateOperandOpcodes = [ ("push imm8", ["push $3"])
                          , ("push imm16", ["push $15000"])
                          , ("push imm32", ["push $1000000000"])
                          , ("Relative short jump (jmp .+0x28)", ["jmp .+0x28"])
                          , ("Relative near jump (jmp .+0xfff)", ["jmp .+0xfff"])
                          ]

mkTest :: (String, [String]) -> T.TestTree
mkTest (name, insns) = T.testCase name $ do
  withAssembledCode insns $ \codeBytes -> do
    let disInsns = D.disassembleBuffer D.defaultX64Disassembler codeBytes
    T.assertEqual "Disassembled instruction count" (length insns) (length disInsns)
    let instances = mapMaybe D.disInstruction disInsns
        assembledInsns = LB.toStrict $ B.toLazyByteString $ mconcat (mapMaybe D.assembleInstruction instances)
    T.assertEqual ("Assembled bytes\n" ++ prettyHex assembledInsns) codeBytes assembledInsns

