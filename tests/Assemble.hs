module Assemble ( assembleTests ) where

import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Flexdis86 as D
import           Flexdis86.Prefixes ( prOSO )

import           Hexdump
import           Util ( withAssembledCode )

assembleTests :: T.TestTree
assembleTests =
  T.testGroup "Assemble Tests" (map mkTest testCases)

j20 :: [D.Value]
j20 = [D.JumpOffset D.JSize8 (D.FixedOffset (20 - 2))]

testCases :: [(String, Maybe D.InstructionInstance)]
testCases = [ ("ret", mkI "ret" [])
            , ("int $0x3", mkI "int3" [])
            , ("push $0x8", mkI "push" [D.ByteImm 8])
            , ("pushw $0xfff", fmap setOSO $ mkI "push" [D.WordImm 0xfff])
            , ("push $0x2000000", mkI "push" [D.DWordImm (D.Imm32Concrete 0x2000000)])
              -- The subtraction here is gross, but required because
              -- the jump is relative to the IP, which is incremented
              -- past the jump by the time it executes.
              --
              -- This only affects us because the assembler would
              -- normally do the rewriting automatically (so we have
              -- to as well)
            , ("jmp .+20", mkI "jmp" j20)
            , ("jmp .+2000", mkI "jmp" [D.JumpOffset D.JSize32 (D.FixedOffset (2000 - 5))])

            -- Warning (64-bit) xors here have multiple encodings --
            -- e.g. @xor %rdx, %rdx@ can be encoded as both 0x4831d2
            -- and 0x4833d2 -- and so these tests depend on flexdis
            -- choosing the same encoding as gcc.
            , ("xor %rdx, %rdx", mkI "xor" [D.QWordReg D.RDX, D.QWordReg D.RDX])
            , ("xor %rbx, %rbx", mkI "xor" [D.QWordReg D.RBX, D.QWordReg D.RBX])
            , ("xor %rcx, %rcx", mkI "xor" [D.QWordReg D.RCX, D.QWordReg D.RCX])
            , ("xor %r8, %r8", mkI "xor" [D.QWordReg (D.Reg64 8), D.QWordReg (D.Reg64 8)])
            , ("movq $0x190000000,%r11", mkI "mov" [D.QWordReg (D.Reg64 11), D.QWordImm 0x190000000])
            , ("movq $0x190000000,%rbx", mkI "mov" [D.QWordReg D.RBX, D.QWordImm 0x190000000])
            , ("sub %rsp,(%rax)", mkI "sub" [D.Mem64 (D.Addr_64 D.DS (Just D.RAX) Nothing D.NoDisplacement), D.QWordReg D.RSP])
            , ("sub (%rax),%rsp", mkI "sub" [D.QWordReg D.RSP, D.Mem64 (D.Addr_64 D.DS (Just D.RAX) Nothing D.NoDisplacement)])

            -- Instructions with mnemonic synonyms.
            , ("jnb .+20", mkI "jnb" j20)
            , ("jnb .+20", mkI "jae" j20)
            , ("jnb .+20", mkI "jnc" j20)
            , ("jae .+20", mkI "jnb" j20)
            , ("jae .+20", mkI "jae" j20)
            , ("jae .+20", mkI "jnc" j20)
            , ("jnc .+20", mkI "jnb" j20)
            , ("jnc .+20", mkI "jae" j20)
            , ("jnc .+20", mkI "jnc" j20)
            ]

setOSO :: D.InstructionInstance -> D.InstructionInstance
setOSO ii = ii { D.iiPrefixes = L.over prOSO (const True) (D.iiPrefixes ii) }

mkI :: String -> [D.Value] -> Maybe D.InstructionInstance
mkI = D.mkInstruction

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
