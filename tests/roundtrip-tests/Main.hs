module Main ( main ) where

import Data.Bits ( Bits )
import qualified Data.ByteString as B
import Data.Maybe ( mapMaybe )
import qualified System.Exit as IO
import qualified System.IO as IO
import qualified System.IO.Temp as IO
import qualified System.Process as P
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Data.Elf as E
import qualified Flexdis86 as D
import qualified Flexdis86.Assembler as A

main :: IO ()
main = T.defaultMain testCases

testCases :: T.TestTree
testCases = T.testGroup "Roundtrip Tests" [
  simpleOpcodes
  ]

simpleOpcodes :: T.TestTree
simpleOpcodes =
  T.testGroup "ZeroOperandOpcodes" (map mkTest zeroOperandOpcodeTests)

zeroOperandOpcodeTests :: [(String, [String])]
zeroOperandOpcodeTests = [ ("ret", ["ret"])
                         , ("x87 wait", ["wait"])
                         , ("int3", ["int $0x3"])
                         , ("nop", ["nop"])
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
                         ]




mkTest :: (String, [String]) -> T.TestTree
mkTest (name, insns) = T.testCase name $ do
  IO.withSystemTempFile "roundtrip.s" $ \fname h -> do
    mapM_ (IO.hPutStrLn h) [ "  .global _start"
                           , "  .text"
                           , "_start:"
                           ]
    mapM_ (IO.hPutStrLn h) (map ("  "++) insns)
    IO.hFlush h
    IO.withSystemTempFile "roundtrip.exe" $ \outfile exeH -> do
      IO.hClose exeH
      let p = P.proc "gcc" ["-nostdlib", "-o", outfile, fname]
      (_, _, _, ph) <- P.createProcess p
      ec <- P.waitForProcess ph
      case ec of
        IO.ExitFailure code -> T.assertFailure ("Assembler failed with exit status " ++ show code)
        IO.ExitSuccess -> do
          codeBytes <- readCodeSegment outfile
          let disInsns = D.disassembleBuffer D.defaultX64Disassembler codeBytes
          T.assertEqual "Disassembled instruction count" (length insns) (length disInsns)
          let instances = mapMaybe D.disInstruction disInsns
              assembledInsns = mapMaybe A.assembleInstruction instances
          T.assertEqual "Assembled bytes" codeBytes (B.concat assembledInsns)

readCodeSegment :: FilePath -> IO B.ByteString
readCodeSegment fp = do
  bytes <- B.readFile fp
  case E.parseElf bytes of
    Left (off, msg) -> error ("Failed to parse ELF file at offset " ++ show off ++ ": " ++ msg)
    Right (E.Elf32 someElf) -> extractCodeSegment someElf
    Right (E.Elf64 someElf) -> extractCodeSegment someElf

extractCodeSegment :: (Bits w, Integral w, E.ElfWidth w) => E.Elf w -> IO B.ByteString
extractCodeSegment e = do
  case E.findSectionByName ".text" e of
    Nothing -> error "Could not find code segment"
    Just textSection -> return $ E.elfSectionData textSection
