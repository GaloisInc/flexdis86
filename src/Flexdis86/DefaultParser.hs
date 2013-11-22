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
  , defaultX64Parser
  ) where

import qualified Data.ByteString as BS
import Language.Haskell.TH.Syntax
import Data.ByteString.Unsafe (unsafePackAddressLen)
import System.IO.Unsafe (unsafePerformIO)

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


defaultX64Parser :: InstructionParser
defaultX64Parser = p
  where Right p = mkX64Parser optableData