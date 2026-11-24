-- | Test that fixed byte sequences disassemble to the expected
-- instruction, using all of the bytes and nothing more.
--
-- Unlike the tests in "Roundtrip" and "Assemble", these tests do not
-- need an assembler, so they run on all platforms. They also cover
-- instructions (e.g., VEX-encoded ones) that the Flexdis86 assembler
-- does not support. The expected text is the output of
-- 'D.ppInstruction', which follows the Intel syntax of
-- @objdump -d -M intel@ up to whitespace.
module Decode ( decodeTests ) where

import qualified Data.ByteString as BS
import           Data.Word ( Word8 )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Flexdis86 as D

decodeTests :: T.TestTree
decodeTests = T.testGroup "Decode Tests"
  [ T.testGroup "VEX vvvv" (map mkTest vexVvvvTests)
  , T.testGroup "Repeated prefixes" (map mkTest repeatedPrefixTests)
  , T.testGroup "32-bit displacements" (map mkDisasmTest displacementTests)
  , T.testGroup "Invalid encodings" (map mkInvalidTest invalidTests)
  ]

-- | VEX instructions that do not use @vvvv@ must have the field
-- encoded as @1111b@.
vexVvvvTests :: [(String, [Word8], String)]
vexVvvvTests =
  [ ("vmovdqa ymm load", [0xc5, 0xfd, 0x6f, 0x06], "vmovdqa ymm0,YMMWORD PTR [rsi]")
  , ("vmovdqa xmm load", [0xc5, 0xf9, 0x6f, 0x06], "vmovdqa xmm0,XMMWORD PTR [rsi]")
  , ("vpshufd ymm", [0xc5, 0xfd, 0x70, 0x06, 0xd8], "vpshufd ymm0,YMMWORD PTR [rsi],0xd8")
  , ("vzeroupper", [0xc5, 0xf8, 0x77], "vzeroupper")
  ]

-- | A prefix byte may be repeated. GCC and Clang pad functions with
-- multi-byte @nop@s such as @data16 cs nop WORD PTR [rax+rax*1+0x0]@.
repeatedPrefixTests :: [(String, [Word8], String)]
repeatedPrefixTests =
  [ ("66 cs nop", 0x66 : csNopW, "nop    [rax+rax*1]")
  , ("66 66 cs nop", 0x66 : 0x66 : csNopW, "nop    [rax+rax*1]")
  , ("66 x5 cs nop", replicate 5 0x66 ++ csNopW, "nop    [rax+rax*1]")
  , ("rep rep stosb", [0xf3, 0xf3, 0xaa], "rep stos BYTE PTR [rdi],al")
  ]
  where
    csNopW = [0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00]

-- | A 32-bit displacement is printed with its sign, like an 8-bit one.
displacementTests :: [([Word8], String)]
displacementTests =
  [ ([0x8b, 0x84, 0x24, 0x20, 0x02, 0x00, 0x00], "mov    eax,DWORD PTR [rsp+0x220]")
  , ([0x89, 0x84, 0x24, 0xfa, 0x00, 0x00, 0x00], "mov    DWORD PTR [rsp+0xfa],eax")
  , ([0x42, 0x89, 0x94, 0x05, 0x10, 0xcf, 0xff, 0xff], "mov    DWORD PTR [rbp+r8*1-0x30f0],edx")
  , ([0x48, 0x8b, 0x05, 0xd0, 0xbc, 0x0d, 0x00], "mov    rax,QWORD PTR [rip+0xdbcd0]")
  , ([0x48, 0x8b, 0x05, 0x10, 0xcf, 0xff, 0xff], "mov    rax,QWORD PTR [rip-0x30f0]")
  ]

-- | Byte sequences that do not encode a valid instruction.
invalidTests :: [(String, [Word8])]
invalidTests =
  [ ("vmovdqa with vvvv /= 1111b", [0xc5, 0xf5, 0x6f, 0x06])
  , ("two different segment prefixes", [0x2e, 0x3e, 0x90])
  ]

mkTest :: (String, [Word8], String) -> T.TestTree
mkTest (name, bytes, expected) = T.testCase name $
  case D.tryDisassemble (BS.pack bytes) of
    (len, Just ii) -> do
      T.assertEqual "Bytes consumed" (length bytes) len
      T.assertEqual "Disassembly" expected (show (D.ppInstruction ii))
    (_, Nothing) -> T.assertFailure ("Failed to disassemble " ++ show bytes)

mkDisasmTest :: ([Word8], String) -> T.TestTree
mkDisasmTest (bytes, expected) = mkTest (expected, bytes, expected)

mkInvalidTest :: (String, [Word8]) -> T.TestTree
mkInvalidTest (name, bytes) = T.testCase name $
  case D.tryDisassemble (BS.pack bytes) of
    (_, Just ii) -> T.assertFailure ("Unexpectedly disassembled " ++ show (D.ppInstruction ii))
    (_, Nothing) -> pure ()
