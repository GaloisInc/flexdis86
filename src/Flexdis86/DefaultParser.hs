{- |
Module      :  $Header$
Copyright   :  (c) 2013-2016 Galois, Inc
Maintainer  :  jhendrix@galois.com

Defines the default parser from optable.xml as compiled in.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy#-}
module Flexdis86.DefaultParser
  ( defaultX64Disassembler
  , defaultX64Assembler
  ) where

import           Control.Monad (when)
import qualified Data.ByteString as BS
import           Data.ByteString.Unsafe (unsafePackAddressLen)
import           Language.Haskell.TH.Syntax
import qualified System.Directory as D
import qualified System.FilePath as F
import           System.IO.Unsafe (unsafePerformIO)

import           Flexdis86.Assembler
import           Flexdis86.Disassembler

{-# NOINLINE optableData #-}

-- | A bytestring containing the compiled XML optable specification.
optableData :: BS.ByteString
optableData =
 -- The @getPathToOptableXML@ computes an absolute path to the XML
 -- file. This is helpful in case our current working directory is not
 -- the directory containing the @flexdis86.cabal@ file. This happens,
 -- e.g., when we build flexdis86 as a dependency of another package
 -- in Emacs @haskell-mode@.
 ($(do let getPathToOptableXML :: IO FilePath
           getPathToOptableXML = do
             let relativePath = "data/optable.xml"
             absPathToThisFile <- D.makeAbsolute __FILE__
             let absolutePath =
                   -- Remove 'src/Flexdis86/DefaultParser.hs' and add
                   -- 'data/optable.xml'.
                   (F.takeDirectory . F.takeDirectory . F.takeDirectory $
                    absPathToThisFile) F.</> relativePath
             exists <- D.doesFileExist absolutePath
             when (not exists) $
               error $ "Can't find \"data/optable.xml\"! Tried " ++
                       absolutePath
             return absolutePath

       path <- qRunIO getPathToOptableXML
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


defaultX64Disassembler :: NextOpcodeTable
defaultX64Disassembler = p
  where p = case mkX64Disassembler optableData of
              Right v -> v
              Left  s -> error ("defaultX64Diassembler: " ++ s)

defaultX64Assembler :: AssemblerContext
defaultX64Assembler =
  case mkX64Assembler optableData of
    Right c -> c
    Left err -> error ("defaultX64Assembler: " ++ err)
