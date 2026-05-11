{- |
Module      : Flexdis86.VEX.Seen
Copyright   : (c) Galois, Inc, 2026

The observed @VEX@ prefix, packed as two raw bytes inside a single
'Word16'. Every constructed 'VEX' is in canonical VEX3 form: VEX2
observations (via 'fromVex2') are normalized on entry by filling in
the implicit @~X=1@, @~B=1@, @W=0@, @m-mmmm=00001@ fields.
Validation against a def's permitted set
('Flexdis86.VEX.Allowed.vexContainedIn') reads 'vexWord' directly -
no per-field extraction.

The sentinel 'noVex' ('VEX' @0@) means \"no VEX prefix observed.\" No
valid VEX encoding can alias this: a real VEX3 byte 1 has
@m-mmmm \\in \\{1, 2, 3\\}@, so its low five bits are always nonzero.

Bit layout (little-endian into the 'Word16'):

@
  byte 0 (low) - VEX3 byte 1
    [7]     ~R
    [6]     ~X
    [5]     ~B
    [4:0]   m-mmmm    (1=0x0F, 2=0x0F 38, 3=0x0F 3A)

  byte 1 (high) - VEX3 byte 2
    [7]     W
    [6:3]   ~vvvv
    [2]     L         (0 = 128-bit, 1 = 256-bit)
    [1:0]   pp        (implied SIMD prefix)
@
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Flexdis86.VEX.Seen
  ( VEX(..)
  , noVex
  , hasVex
  , fromVex2
  , fromVex3
    -- * Escape bytes
  , vex2EscapeByte
  , vex3EscapeByte
    -- * Accessors
  , vexRex
  , vexVVVV
  , vex256
  , vexByte1
  , vexByte2
    -- * Raw access
  , vexWord
  ) where

import           Control.Exception (assert)
import qualified Control.DeepSeq as DS
import           Data.Binary (Binary)
import           Data.Bits ( (.&.), (.|.), complement, shiftL, shiftR
                           , testBit
                           )
import           Data.Word (Word8, Word16)

import           Flexdis86.Prefixes.REX (REX(..))

-- | Packed VEX prefix. @'VEX' 0@ is the distinguished \"no VEX\"
-- value; any other 'Word16' encodes a VEX3-normalized pair of raw VEX
-- bytes. See the module haddock for the exact layout.
newtype VEX = VEX Word16
  deriving (Binary, DS.NFData, Eq, Ord, Show)

-- | The \"no VEX prefix observed\" sentinel. Equal to @'VEX' 0@;
-- compare with @== 'noVex'@ or @/= 'noVex'@ to branch on presence.
noVex :: VEX
noVex = VEX 0
{-# INLINE noVex #-}

-- | Is a VEX prefix present?
hasVex :: VEX -> Bool
hasVex v = v /= noVex
{-# INLINE hasVex #-}

-- | Escape byte introducing a two-byte VEX prefix (one payload byte
-- follows).
vex2EscapeByte :: Word8
vex2EscapeByte = 0xC5

-- | Escape byte introducing a three-byte VEX prefix (two payload bytes
-- follow).
vex3EscapeByte :: Word8
vex3EscapeByte = 0xC4

-- | Build a 'VEX' from a VEX2 payload byte (the byte following a
-- 'vex2EscapeByte'). Normalized to VEX3 form in-place: byte 1 is @~R@
-- from the payload's high bit with @~X=1@, @~B=1@, @m-mmmm=00001@; byte
-- 2 is the payload with @W=0@ (VEX2 implies @W=0@).
fromVex2 :: Word8 -> VEX
fromVex2 payload =
  packBytes ((payload .&. 0x80) .|. 0x61) (payload .&. 0x7F)
{-# INLINE fromVex2 #-}

-- | Build a 'VEX' from the two VEX3 payload bytes (the bytes following
-- a 'vex3EscapeByte').
fromVex3 :: Word8 -> Word8 -> VEX
fromVex3 = packBytes
{-# INLINE fromVex3 #-}

packBytes :: Word8 -> Word8 -> VEX
packBytes b1 b2 =
  VEX (fromIntegral @Word8 @Word16 b1 .|. (fromIntegral @Word8 @Word16 b2 `shiftL` 8))
{-# INLINE packBytes #-}

-- | Raw VEX3 byte 1. Zero when 'hasVex' is 'False'.
vexByte1 :: VEX -> Word8
vexByte1 (VEX w) = fromIntegral @Word16 @Word8 w
{-# INLINE vexByte1 #-}

-- | Raw VEX3 byte 2. Zero when 'hasVex' is 'False'.
vexByte2 :: VEX -> Word8
vexByte2 (VEX w) = fromIntegral @Word16 @Word8 (w `shiftR` 8)
{-# INLINE vexByte2 #-}

------------------------------------------------------------------------
-- Accessors
--
-- For a canonical VEX3 encoding (which is what we always store), the
-- REX, vvvv, L, and pp fields are pure bit extractions. No case on
-- VEX2 vs VEX3 is needed because 'mkVEX2' fills in VEX3's implicit
-- fields.

-- | The implied REX byte. Combines @~R@\/@~X@\/@~B@ from byte 1 and
-- @W@ from byte 2, inverting and shifting into the REX byte's
-- @0b0100WRXB@ layout.
--
-- Precondition: 'hasVex'. Asserted, but not checked in production.
vexRex :: VEX -> REX
vexRex v@(VEX w) =
  assert (hasVex v) $
  let b1 = fromIntegral @Word16 @Word8 w
      b2 = fromIntegral @Word16 @Word8 (w `shiftR` 8)
  in REX ( 0x40
      .|. (complement b2 .&. 0x80) `shiftR` 4         -- W from byte 2 bit 7 -> REX bit 3
      .|. (complement b1 .&. 0xE0) `shiftR` 5         -- ~R ~X ~B -> REX bits 2..0
         )
{-# INLINE vexRex #-}

-- | The 4-bit @vvvv@ source register (de-inverted). Callers that
-- interpret it as a register index typically complement and mask.
--
-- Precondition: 'hasVex'. Asserted, but not checked in production.
vexVVVV :: VEX -> Word8
vexVVVV v = assert (hasVex v) $ (vexByte2 v `shiftR` 3) .&. 0xF
{-# INLINE vexVVVV #-}

-- | Is this a 256-bit vector operation (L bit of byte 2)?
--
-- Precondition: 'hasVex'. Asserted, but not checked in production.
vex256 :: VEX -> Bool
vex256 v = assert (hasVex v) $ testBit (vexByte2 v) 2
{-# INLINE vex256 #-}

------------------------------------------------------------------------
-- Raw access

-- | The raw packed VEX3-normalized bytes as a single 'Word16'. Used by
-- 'Flexdis86.VEX.Allowed.vexContainedIn' for direct bitmask validation.
vexWord :: VEX -> Word16
vexWord (VEX w) = w
{-# INLINE vexWord #-}
