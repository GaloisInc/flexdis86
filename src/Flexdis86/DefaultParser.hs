{- |
Module      :  $Header$
Copyright   :  (c) 2013-2016 Galois, Inc
Maintainer  :  jhendrix@galois.com

Defines the default parser from optable.xml as compiled in.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy#-}
{-# OPTIONS_GHC -ddump-splices #-}
module Flexdis86.DefaultParser
  ( defaultX64Disassembler
  , defaultX64Assembler
  ) where

-- TODO RGS: Explain why we need this
import           Instances.TH.Lift ()

import           Flexdis86.Assembler
import           Flexdis86.DefaultParser.TH
import           Flexdis86.Disassembler

defaultX64Disassembler :: NextOpcodeTable
defaultX64Disassembler =
  $(do case mkX64Disassembler optableData of
         Right v -> [| v |]
         Left  s -> error ("defaultX64Diassembler: " ++ s))

defaultX64Assembler :: AssemblerContext
defaultX64Assembler =
  $(do case mkX64Assembler optableData of
         Right c -> [| c |]
         Left err -> error ("defaultX64Assembler: " ++ err))
