{- |
Module      :  $Header$
Copyright   :  (c) 2013-2016 Galois, Inc
Maintainer  :  jhendrix@galois.com

Defines the default parser from optable.xml as compiled in.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
module Flexdis86.DefaultParser
  ( defaultX64Disassembler
  , defaultX64Assembler
  ) where

import           Control.Monad (when)
import qualified Data.ByteString as BS
import           Instances.TH.Lift ()
import           Language.Haskell.TH.Syntax (lift, qAddDependentFile, qRunIO)
import qualified System.Directory as D
import qualified System.FilePath as F

import           Flexdis86.Assembler
import           Flexdis86.Disassembler
import           Flexdis86.OpTable

-- | The instruction definitions parsed from @optable.xml@ at compile time.
-- Only definitions supported by flexdis86 are included (see 'defSupported').
optableDefs :: [Def]
optableDefs =
 -- The @getPathToOptableXML@ computes an absolute path to the XML
 -- file. This is helpful in case our current working directory is not
 -- the directory containing the @flexdis86.cabal@ file. This happens,
 -- e.g., when we build flexdis86 as a dependency of another package
 -- in Emacs @haskell-mode@.
 $(do let getPathToOptableXML :: IO FilePath
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
      case parseOpTable contents of
        Left e    -> error ("optableDefs: failed to parse optable.xml: " ++ e)
        Right defs -> lift defs)


defaultX64Disassembler :: NextOpcodeTable
defaultX64Disassembler = p
  where p = case mkX64Disassembler optableDefs of
              Right v -> v
              Left  s -> error ("defaultX64Disassembler: " ++ s)

defaultX64Assembler :: AssemblerContext
defaultX64Assembler = mkX64Assembler optableDefs
