{- |
Module      :  $Header$
Description :  Defines the default parser from optable.xml as compiled in.
Copyright   :  (c) Galois, Inc
Maintainer  :  jhendrix@galois.com
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Flexdis86.DefaultParser
  ( optableData
  , defaultX64Disassembler
  , defaultX64Assembler
  ) where

import qualified Data.ByteString as BS
import Language.Haskell.TH.Syntax
import Data.ByteString.Unsafe (unsafePackAddressLen)
import System.IO.Unsafe (unsafePerformIO)

import Flexdis86.Assembler
import Flexdis86.Disassembler

{-# NOINLINE optableData #-}

optableData :: BS.ByteString
optableData = 
 ($(do let path = "data/optable.xml"
       qAddDependentFile path
       contents <- qRunIO $ BS.readFile path
       let blen :: Int
           blen = fromIntegral (BS.length contents)
#if MIN_VERSION_template_haskell(2,8,0)
       let addr = LitE $ StringPrimL $ BS.unpack contents
#else
       let addr = LitE $ StringPrimL $ UTF8.toString contents
#endif
       [| unsafePerformIO $ unsafePackAddressLen blen $(return addr) |]))


defaultX64Disassembler :: InstructionParser
defaultX64Disassembler = p
  where p = case mkX64Disassembler optableData of
              Right v -> v
              Left  s -> error ("defaultX64Diassembler: " ++ s)

defaultX64Assembler :: AssemblerContext
defaultX64Assembler =
  case mkX64Assembler optableData of
    Right c -> c
    Left err -> error ("defaultX64Assembler: " ++ err)
