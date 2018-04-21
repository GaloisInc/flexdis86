{- |
Module      :  $Header$
Copyright   :  (c) Galois, Inc 2018
Maintainer  :  jhendrix@galois.com

This declares types needed to support relocations.
-}
module Flexdis86.Relocation
  ( -- * Jump
    JumpSize(..)
  , JumpOffset(..)
    -- * Immediates
  , Imm32(..)
  ) where


import Data.BinarySymbols
import Data.Int
import Data.Word
import Numeric

------------------------------------------------------------------------
-- JumpOffset

-- | The number of bytes in a jump.  Values are always signed bitvectors.
data JumpSize
   = JSize8
   | JSize16
   | JSize32
 deriving (Eq, Ord, Show)

-- | A jump target
data JumpOffset
  = FixedOffset !Int64
    -- ^ This denotes a static offset with the given size.
  | RelativeOffset !SymbolIdentifier !Word32 !Int64
    -- ^ @RelativeOffset sym ioff off@ denotes a relative address.
    --
    -- @ioff@ stores the number of bytes read in the instruction
    -- before reading the relative offset.
    -- The computed value should be @addr(sym) + off - (initPC + ioff)@ where
    -- @initPC is the base address that @ioff@ is relative to.  In macaw-x86, @initPC@
    -- is the PC value of the instruction, but it can technically be other offsets.

data Imm32
   = Imm32Concrete !Word32
    -- ^ @Imm32Concrete c@ denotes the value of @c@,
   | Imm32SymbolOffset !SymbolIdentifier !Int64
    -- ^ @Imm32SymbolOffset sym off@ denotes the value of @addr(sym) + off@.
    --
    -- This is required to be a 32-bit value.  If not, then the instruction is malformed.


instance Show Imm32 where
  showsPrec _ (Imm32Concrete c) = showString "0x" . showHex c
  showsPrec _ (Imm32SymbolOffset s o)
    = showString "[roff"
    . shows s
    . showChar ','
    . shows o
    . showChar ']'

showOff :: Int64 -> ShowS
showOff i
  | i < 0 = showChar '-' . showHex (negate i)
  | otherwise = showHex i

instance Show JumpOffset where
  showsPrec _ (FixedOffset i) = showOff i
  showsPrec _ (RelativeOffset sym ioff off)
    = showString "[roff"
    . shows ioff
    . showChar ','
    . shows sym
    . showChar ','
    . showOff off
    . showChar ']'
