{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2016
Maintainer  : jhendrix@galois.com

Top-level module for Flexdis.
-}
module Flexdis86
  ( module Flexdis86.InstructionSet
  , module Flexdis86.Prefixes
  , module Flexdis86.Register
  , module Flexdis86.Segment
  , module Flexdis86.Operand
    -- * Disassembler
  , disassembleInstruction
  , tryDisassemble
  , disassembleBuffer
  , D.DisassembledAddr(..)
  , module Flexdis86.ByteReader
    -- * Assembler
  , mkInstruction
  , A.assembleInstruction
  ) where

import           Control.Monad ( MonadPlus )
import qualified Data.ByteString as B

import qualified Flexdis86.Assembler as A
import           Flexdis86.ByteReader
import           Flexdis86.DefaultParser
import qualified Flexdis86.Disassembler as D
import           Flexdis86.InstructionSet
import           Flexdis86.Operand
import           Flexdis86.Prefixes
import           Flexdis86.Register
import           Flexdis86.Segment

-- | Parse instruction using byte reader.
disassembleInstruction :: ByteReader m
                       => m InstructionInstance
disassembleInstruction = D.disassembleInstruction defaultX64Disassembler

-- | Try disassemble returns the numbers of bytes read and an instruction instance.
tryDisassemble :: B.ByteString -> (Int, Maybe InstructionInstance)
tryDisassemble = D.tryDisassemble defaultX64Disassembler

disassembleBuffer :: B.ByteString
                     -- ^ Buffer to decompose
                  -> [D.DisassembledAddr]
disassembleBuffer = D.disassembleBuffer defaultX64Disassembler

-- | Create zero or more instruction instances from a string and arguments.
mkInstruction :: MonadPlus m
              => String
                 -- ^ Mnemonic
              -> [Value]
                 -- ^ Arguments
              -> m InstructionInstance
mkInstruction = A.mkInstruction defaultX64Assembler
