module Util ( withAssembledCode ) where

import Data.Bits ( Bits )
import qualified Data.ByteString as B
import qualified System.Exit as IO
import qualified System.IO as IO
import qualified System.IO.Temp as IO
import qualified System.Process as P
import qualified Test.Tasty.HUnit as T

import qualified Data.ElfEdit as E

-- | Put the given assembly instructions into an assembly file,
-- assemble it, then extract the bytes from the code segment.  Feed
-- those bytes to a callback.
withAssembledCode :: [String] -> (B.ByteString -> IO ()) -> IO ()
withAssembledCode insns k = do
  IO.withSystemTempFile "asm.s" $ \fname h -> do
    mapM_ (IO.hPutStrLn h) [ "  .global _start"
                           , "  .text"
                           , "_start:"
                           ]
    mapM_ (IO.hPutStrLn h) (map ("  "++) insns)
    IO.hFlush h
    IO.withSystemTempFile "asm.exe" $ \outfile exeH -> do
      IO.hClose exeH
      let p = P.proc "gcc" ["-nostdlib", "-o", outfile, fname]
      (_, _, _, ph) <- P.createProcess p
      ec <- P.waitForProcess ph
      case ec of
        IO.ExitFailure code -> T.assertFailure ("Assembler failed with exit status " ++ show code)
        IO.ExitSuccess -> do
          codeBytes <- readCodeSegment outfile
          k codeBytes

readCodeSegment :: FilePath -> IO B.ByteString
readCodeSegment fp = do
  bytes <- B.readFile fp
  case E.parseElf bytes of
    Left (off, msg) -> error ("Failed to parse ELF file at offset " ++ show off ++ ": " ++ msg)
    Right (E.Elf32 someElf) -> extractCodeSegment someElf
    Right (E.Elf64 someElf) -> extractCodeSegment someElf

extractCodeSegment :: (Bits w, Integral w) => E.Elf w -> IO B.ByteString
extractCodeSegment e = do
  case E.findSectionByName ".text" e of
    [] -> error "extractCodeSegment: Could not find code segment"
    [textSection] -> return $ E.elfSectionData textSection
    _ -> error "extractCodeSegment: Too many text segments"
