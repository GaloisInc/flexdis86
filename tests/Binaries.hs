{-# LANGUAGE OverloadedStrings #-}

module Binaries ( binaryTests ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import           Data.ElfEdit as ElfEdit
import           Data.Foldable ( for_ )
import           Data.Maybe ( catMaybes, fromMaybe, mapMaybe )
import qualified System.Directory as Directory
import           System.Environment ( lookupEnv )
import           System.FilePath ( (</>) )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Flexdis86 as D

getTextSection :: FilePath -> IO B.ByteString
getTextSection binPath = do
  elfBS <- B.readFile binPath
  let elf64 =
        case ElfEdit.parseElf elfBS of
          ElfEdit.Elf64Res _ elf -> elf
          _ -> error "Couldn't parse ELF"
  case ElfEdit.findSectionByName ".text" elf64 of
    [sec] -> pure $ ElfEdit.elfSectionData sec
    _ -> error "Couldn't find single .text section"

binaryTests :: T.TestTree
binaryTests = T.testCase "Disassemble/reassemble binaries" $ do
  let hasExt ext = (== ext) . reverse . take (length ext) . reverse
  binDir <- fromMaybe "deps/sample-binaries/tiny/" <$>
    lookupEnv "FLEXDIS_SAMPLE_BINARIES"
  bins <- filter (hasExt ".x86_64-exe") <$> Directory.listDirectory binDir
  T.assertBool
    ("Enough tests (" ++ show (length bins) ++ ")")
    (length bins > 0)
  for_ bins $ \binPath -> do
    let minLength = 0
    codeBytes <- getTextSection (binDir </> binPath)
    let disBuf = D.disassembleBuffer codeBytes
    T.assertBool
      ("Disassembled something in " ++ binPath)
      (length disBuf > minLength)
    let disIns = map D.disInstruction disBuf
    T.assertBool
      ("Disassembled some instructions in " ++ binPath)
      (length (catMaybes disIns) > minLength)

    -- Roundtrip: disassemble, reassemble, then re-disassemble and compare
    -- instructions. We compare at the instruction level rather than raw bytes
    -- because some compilers emit legacy prefixes in non-canonical order (e.g.
    -- OSO before segment override) while the assembler always emits canonical
    -- order. Both encodings are architecturally equivalent and disassemble to
    -- the same InstructionInstance.
    let origIns = mapMaybe D.disInstruction disBuf
    T.assertEqual
      ("Every position disassembled in " ++ binPath)
      (length disBuf)
      (length origIns)
    asIns <- traverse D.assembleInstruction origIns
    let assembledBS = LB.toStrict $ BB.toLazyByteString $ mconcat asIns
    let redisIns = mapMaybe D.disInstruction (D.disassembleBuffer assembledBS)
    T.assertEqual
      ("Roundtrip instructions in " ++ binPath)
      origIns
      redisIns
