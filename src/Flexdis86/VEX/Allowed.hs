{- |
Module      : Flexdis86.VEX.Allowed
Copyright   : (c) Galois, Inc, 2026

The set of VEX encodings a 'Flexdis86.OpTable.Def' permits, packed as
a @(mask, expected)@ pair that is matched directly against the raw
VEX 'Word16' bytes from 'Flexdis86.VEX.Seen.VEX'. Every VEX field in
the optable is either \"fixed to one value\" or \"don't care\", so
the check collapses to

@
  v .&. mask == expected
@

Layout (mirrors the raw VEX 'Word16' in 'Flexdis86.VEX.Seen.VEX'):

@
  byte 0 - VEX3 byte 1
    [7]     ~R           never fixed
    [6]     ~X           never fixed
    [5]     ~B           never fixed
    [4:0]   m-mmmm       always fixed (mask=0x1F)

  byte 1 - VEX3 byte 2
    [7]     W            fixed for W0\/W1, wildcard for WIG
    [6:3]   ~vvvv        fixed to 0 when vvvv=15, wildcard otherwise
    [2]     L            always fixed
    [1:0]   pp           always fixed
@

The 'noVexAllowed' sentinel is @'Allowed' 0 0@: since any real VEX
observation has @m-mmmm /= 0@, @v .&. 0 == 0@ only holds when @v@ is
the 'noVex' sentinel, so non-VEX defs validate against \"no VEX seen\"
and reject any real VEX bytes.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Flexdis86.VEX.Allowed
  ( Allowed
  , noVexAllowed
  , hasVexAllowed
  , vexContainedIn
    -- * Construction
  , allowVex
  , VexMmmm(..)
  , VexPp(..)
  , VexL(..)
  , VexW(..)
  ) where

import           Control.DeepSeq (NFData(rnf))
import           Data.Binary (Binary(get, put))
import           Data.Bits ( (.&.), (.|.), shiftL )
import           Data.Word (Word8, Word16)

import qualified Flexdis86.VEX.Seen as VEX

-- | Packed VEX-encoding constraint for a 'Def'. See the module
-- haddock for the layout.
data Allowed = Allowed {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
  deriving (Eq, Ord, Show)

-- | Strict in both fields - the constructor already is, so 'rnf' is a
-- no-op.
instance NFData Allowed where
  rnf !_ = ()

-- | Big-endian pair of 'Word16's - matches the 'Data.Binary' default
-- for 'Word16'.
instance Binary Allowed where
  put (Allowed m e) = put m *> put e
  get = Allowed <$> get <*> get

-- | \"No VEX prefix is permitted for this Def.\" With mask=0 and
-- expected=0, @v .&. 0 == 0@ is 'True' only for @v = 'VEX.noVex'@,
-- so this matches a missing VEX observation and rejects any real one.
noVexAllowed :: Allowed
noVexAllowed = Allowed 0 0
{-# INLINE noVexAllowed #-}

-- | Does this 'Allowed' permit any VEX encoding at all? True iff the
-- mask pins any bits (non-VEX defs use mask=0).
hasVexAllowed :: Allowed -> Bool
hasVexAllowed (Allowed m _) = m /= 0
{-# INLINE hasVexAllowed #-}

-- | Does the observed VEX validate against an 'Allowed' mask? Two
-- arithmetic ops: @v .&. mask == expected@. The caller is responsible
-- for only invoking this with a real VEX when the def requires one
-- (the split-trie decoder does so structurally - see
-- @Note [x86_64 disassembly]@).
vexContainedIn :: VEX.VEX -> Allowed -> Bool
vexContainedIn seen (Allowed mask expected) =
  VEX.vexWord seen .&. mask == expected
{-# INLINE vexContainedIn #-}

------------------------------------------------------------------------
-- Construction

-- | VEX @m-mmmm@ field: which opcode-escape prefix the instruction
-- implies.
data VexMmmm = M0x0F | M0x0F38 | M0x0F3A
  deriving (Eq, Show)

-- | VEX @pp@ field: the implied legacy SIMD prefix.
data VexPp = PpNone | Pp0x66 | Pp0xF3 | Pp0xF2
  deriving (Eq, Show)

-- | VEX @L@ field: vector length.
data VexL = L128 | L256
  deriving (Eq, Show)

-- | VEX @W@ field: 'WIG' means the def accepts both @W=0@ and @W=1@.
data VexW = W0 | W1 | WIG
  deriving (Eq, Show)

mmmmBits :: VexMmmm -> Word8
mmmmBits = \case
  M0x0F   -> 1
  M0x0F38 -> 2
  M0x0F3A -> 3

ppBits :: VexPp -> Word8
ppBits = \case
  PpNone -> 0
  Pp0x66 -> 1
  Pp0xF3 -> 2
  Pp0xF2 -> 3

lBits :: VexL -> Word8
lBits = \case
  L128 -> 0
  L256 -> 1

-- | Build the 'Allowed' mask for a single VEX-permitting 'Def'.
allowVex
  :: VexMmmm
  -> VexPp
  -> VexL
  -> VexW
  -> Bool
     -- ^ @'True'@ iff @vvvv@ is fixed at 15 (i.e.\ @~vvvv = 0@);
     --   @'False'@ iff any @vvvv@ in @0..15@ is permitted
  -> Allowed
allowVex m p l w vvvvFix15 = Allowed mask expected
  where
    -- byte 0: m-mmmm in [4:0]; ~R ~X ~B in [7:5] are never fixed.
    mask0, exp0 :: Word8
    mask0 = 0b00011111
    exp0  = mmmmBits m

    -- byte 1: pp in [1:0], L in [2], ~vvvv in [6:3], W in [7].
    mask1, exp1 :: Word8
    mask1 = 0b00000111                              -- pp + L always fixed
        .|. (if vvvvFix15 then 0b01111000 else 0)   -- ~vvvv = 0
        .|. (case w of WIG -> 0; _ -> 0b10000000)   -- W
    exp1  = ppBits p
        .|. (lBits l `shiftL` 2)
        .|. (case w of W0 -> 0; W1 -> 1 `shiftL` 7; WIG -> 0)

    mask     = pack mask0 mask1
    expected = pack exp0  exp1

pack :: Word8 -> Word8 -> Word16
pack lo hi = fromIntegral @Word8 @Word16 lo
         .|. (fromIntegral @Word8 @Word16 hi `shiftL` 8)
{-# INLINE pack #-}
