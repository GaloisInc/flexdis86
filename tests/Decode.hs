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

-- | Byte sequences that do not encode a valid instruction.
invalidTests :: [(String, [Word8])]
invalidTests =
  [ ("vmovdqa with vvvv /= 1111b", [0xc5, 0xf5, 0x6f, 0x06])
  ]

mkTest :: (String, [Word8], String) -> T.TestTree
mkTest (name, bytes, expected) = T.testCase name $
  case D.tryDisassemble (BS.pack bytes) of
    (len, Just ii) -> do
      T.assertEqual "Bytes consumed" (length bytes) len
      T.assertEqual "Disassembly" expected (show (D.ppInstruction ii))
    (_, Nothing) -> T.assertFailure ("Failed to disassemble " ++ show bytes)

mkInvalidTest :: (String, [Word8]) -> T.TestTree
mkInvalidTest (name, bytes) = T.testCase name $
  case D.tryDisassemble (BS.pack bytes) of
    (_, Just ii) -> T.assertFailure ("Unexpectedly disassembled " ++ show (D.ppInstruction ii))
    (_, Nothing) -> pure ()
