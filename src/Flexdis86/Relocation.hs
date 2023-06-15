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
  , UImm64(..)
  ) where

import Data.BinarySymbols
import Data.Int
import Data.String (fromString)
import Data.Word
import Numeric
import qualified Prettyprinter as PP

------------------------------------------------------------------------
-- JumpOffset

-- | The number of bytes in a jump.  Values are always signed bitvectors.
data JumpSize
   = JSize8
   | JSize16
   | JSize32
 deriving (Eq, Ord, Show)

-- | This describes an amount to add to the PC to jump to a new location.
data JumpOffset
  = FixedOffset !Int64
    -- ^ This denotes a static offset with the given size.
  | RelativeOffset !Word32 !SymbolIdentifier !Int64
    -- ^ @RelativeOffset ioff sym off@ denotes a relative address.
    --
    -- @ioff@ stores the number of bytes read in the instruction
    -- before reading the relative offset.
    -- The computed value should be @addr(sym) + off - (initPC + ioff)@ where
    -- @initPC is the base address that @ioff@ is relative to.  In macaw-x86, @initPC@
    -- is the PC value of the instruction, but it can technically be other offsets.
    --
    -- Since this is added to the current PC value, which will point to the end of the instruction,
    -- the effective address written will be addr(sym) + off + (instructionLength - ioff)
  deriving (Eq, Ord)

-- | A 32-bit value which could either be a specific number, or a
-- relocation that should be computed at later load/link time.
data Imm32
   = Imm32Concrete !Int32
    -- ^ @Imm32Concrete c@ denotes the value of @c@,
   | Imm32SymbolOffset !SymbolIdentifier !Int64 !Bool
    -- ^ @Imm32SymbolOffset sym off signed@ denotes the value of @addr(sym) + off@.
    --
    -- We can assume that the computed value is in `[0..2^32-)` if `signed` is false,
    -- and `[-2^31..2^31)` if `signed` is true.  If not, the relocation should fail
    -- to apply prior to program execution.
  deriving (Eq, Ord)

instance Show Imm32 where
  showsPrec _ (Imm32Concrete c) =
    if c < 0 then
      -- convert to Integer before negate (negate minBound == minBound :: Int32)
      showString "-0x" . showHex (negate $ toInteger c)
     else
      showString "0x" . showHex c
  showsPrec _ (Imm32SymbolOffset s o isSigned)
    = showString "[roff"
    . shows s
    . showChar ','
    . shows o
    . (if isSigned then showString ",S" else id)
    . showChar ']'

instance PP.Pretty Imm32 where pretty = fromString . show

-- | A 32-bit value which could either be a specific number, or a
-- relocation that should be computed at later load/link time.
data UImm64
   = UImm64Concrete !Word64
    -- ^ @UImm64Concrete c@ denotes the value of @c@,
   | UImm64SymbolOffset !SymbolIdentifier !Int64
    -- ^ @UImm64SymbolOffset sym off@ denotes the value of @addr(sym) + off@.
    --
    -- We can assume that the computed value is in `[0..2^64-)`.  If not, the
    -- relocation should fail to apply prior to program execution.
  deriving (Eq, Ord)

instance Show UImm64 where
  showsPrec _ (UImm64Concrete c) = showString "0x" . showHex c
  showsPrec _ (UImm64SymbolOffset s o)
    = showString "[areloc"
    . shows s
    . showChar ','
    . shows o
    . showChar ']'

instance PP.Pretty UImm64 where pretty = fromString . show

showOff :: Int64 -> ShowS
showOff i
  -- convert to Integer before negate (negate minBound == minBound :: Int64)
  | i < 0 = showChar '-' . showHex (negate $ toInteger i)
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

instance PP.Pretty JumpOffset where pretty = fromString . show
