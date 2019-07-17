{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
module Util
  ( AsmFlavor(..)
  , withAssembledCode
  ) where

import           Control.Monad ( when )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified System.IO as IO
import qualified System.IO.Temp as IO
import qualified System.Process as P
import qualified Test.Tasty.HUnit as T

import qualified Data.ElfEdit as E

start_sym_name :: String
start_sym_name = "_start"

readCodeSegment :: FilePath -> IO B.ByteString
readCodeSegment fp = do
  bytes <- B.readFile fp
  E.parseElfOrDie extractCodeSegment extractCodeSegment bytes

extractCodeSegment :: E.Elf w -> IO B.ByteString
extractCodeSegment e = do
  case E.findSectionByName (B8.pack ".text") e of
    [] -> error "extractCodeSegment: Could not find code segment"
    [textSection] -> return $ E.elfSectionData textSection
    _ -> error "extractCodeSegment: Too many text segments"

data AsmFlavor = Intel | Att deriving (Eq, Show)

-- | Put the given assembly instructions into an assembly file,
-- assemble it, then extract the bytes from the code segment.  Feed
-- those bytes to a callback.
withAssembledCode :: AsmFlavor -> [String] -> (B.ByteString -> IO ()) -> IO ()
withAssembledCode flavor insns k = do
  IO.withSystemTempFile "asm.s" $ \fname h -> do
    when (flavor == Intel) $ do
      IO.hPutStrLn h "  .intel_syntax noprefix"
    mapM_ (IO.hPutStrLn h) [ "  .global " ++ start_sym_name
                           , "  .text"
                           , start_sym_name ++ ":"
                           ]
    mapM_ (IO.hPutStrLn h) (map ("  "++) insns)
    IO.hFlush h
    IO.withSystemTempFile "asm.exe" $ \outfile exeH -> do
      IO.hClose exeH
      let args = ["-nostdlib", "-static", "-o", outfile, fname]
      let p = P.proc "gcc" args
      (_, _, _, ph) <- P.createProcess p
      ec <- P.waitForProcess ph
      case ec of
        IO.ExitFailure code -> do
          _ <- IO.exitFailure
          T.assertFailure $
             "Assembler failed with exit status " ++ show code ++ "\n"
             ++ "Cmd: gcc " ++ unwords args
        IO.ExitSuccess -> do
          codeBytes <- readCodeSegment outfile
          k codeBytes
