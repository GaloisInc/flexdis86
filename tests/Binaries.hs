{-# LANGUAGE OverloadedStrings #-}

module Binaries ( binaryTests ) where

import           Data.ElfEdit as ElfEdit
import           Data.Foldable ( for_ )
import qualified Data.ByteString as LB
import           Data.Maybe ( catMaybes, fromMaybe )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified System.Directory as Directory
import           System.Environment ( lookupEnv )
import           System.IO.Temp ( withSystemTempFile )
import qualified System.IO as IO

import qualified Flexdis86 as D

getTextSection :: FilePath -> IO LB.ByteString
getTextSection binPath = do
  elfBS <- LB.readFile binPath
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
    codeBytes <- getTextSection (binDir ++ binPath)
    let disBuf = D.disassembleBuffer codeBytes
    T.assertBool
      ("Disassembled something in " ++ binPath)
      (length disBuf > minLength)
    let disIns = map D.disInstruction disBuf
    T.assertBool
      ("Disassembled some instructions in " ++ binPath)
      (length (catMaybes disIns) > minLength)


    -- Force full evaluation by writing to a file
    withSystemTempFile "flexdis86-disassemble-test.txt" $ \_fp handle -> do
      -- For debugging:
      -- putStrLn $ "In file " ++ binPath
      for_ disBuf $ \instr -> do
        -- putStrLn $ "At offset " ++ (show (D.disOffset instr))
        IO.hPutStrLn handle (show (D.disOffset instr))

    -- TODO(lb): It would be nice if we could roundtrip all these binaries, but
    -- at the moment some fail. This would also obviate the hack of writing to
    -- file above.

    -- case sequence disIns of
    --   Nothing -> fail $ "Not everything got disassembled: " ++ binPath
    --   Just disIns' -> do
    --     asIns <- traverse D.assembleInstruction disIns'
    --     let assembledInsns = B.toLazyByteString . mconcat $ asIns
    --     T.assertEqual "Roundtrip" codeBytes (LB.toStrict assembledInsns)
