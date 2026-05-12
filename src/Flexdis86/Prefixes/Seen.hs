{- |
Module      : Flexdis86.Prefixes.Seen
Copyright   : (c) Galois, Inc, 2026

Accumulates which prefix bytes have been observed in the byte stream during
decode, represented as a 'Flexdis86.Prefixes.Code.PrefixCode'. Validation
against a 'Flexdis86.Prefixes.Allowed.Allowed' is one 'containedIn' call;
see "Flexdis86.Prefixes.Code" for the shared bit layout.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Flexdis86.Prefixes.Seen
  ( Seen
  , seenCode
  , emptySeen
  , addPrefix
  , checkAllowed
  ) where

import qualified Control.DeepSeq as DS
import           Data.Binary (Binary)
import           Data.Bits (Bits, (.&.), (.|.), zeroBits)
import           Data.Coerce (coerce)
import           Data.Word (Word8, Word16)

import           Flexdis86.Prefixes.Allowed
import           Flexdis86.Prefixes.Bytes
import           Flexdis86.Prefixes.Code
import           Flexdis86.Prefixes.Required
import qualified Flexdis86.VEX.Seen as VEX

-- | What prefix bytes have been observed in the byte stream. Wraps a
-- 'PrefixCode'; see "Flexdis86.Prefixes.Code" for the bit layout. The
-- two validation-excluded bits (13, 14) are never set on a 'Seen'.
newtype Seen = Seen { seenCode :: PrefixCode }
  deriving (Bits, Binary, DS.NFData, Eq, Ord, Show)

-- | An empty 'Seen' value: no prefix bytes observed.
emptySeen :: Seen
emptySeen = Seen emptyPrefixCode

-- | Fold a legacy prefix byte into a 'Seen' value. Returns 'Nothing' if
-- the byte is not a legacy prefix, or if its prefix group is already
-- occupied (duplicate prefixes within the same group are rejected).
addPrefix :: Seen -> Word8 -> Maybe Seen
addPrefix !(Seen seen) b = coerce (addPrefixCode seen b)
{-# INLINE addPrefix #-}

-- Internal: the real work, operating on raw 'PrefixCode'.
addPrefixCode :: PrefixCode -> Word8 -> Maybe PrefixCode
addPrefixCode !seen b
  | b == lockPrefixByte = group lockRepMask lockBit
  | b == repNZPrefixByte = group lockRepMask repNZBit
  | b == repPrefixByte = group lockRepMask repBit
  | b == operandSizeOverrideByte = group osoBit osoBit
  | b == addrSizeOverrideByte = group asoBit asoBit
  | b == esPrefixByte = group segMask segEs
  | b == csPrefixByte = group segMask segCs
  | b == ssPrefixByte = group segMask segSs
  | b == dsPrefixByte = group segMask segDs
  | b == fsPrefixByte = group segMask segFs
  | b == gsPrefixByte = group segMask segGs
  | isRexPrefixByte b = addRex b
  | otherwise = Nothing
  where
    lockRepMask = lockBit .|. repNZBit .|. repBit
    -- Set the bits of @bits@ in the accumulator, but only if every bit
    -- of the group mask is currently clear. Duplicate prefixes within a
    -- group are rejected.
    group mask bits
      | seen .&. mask == zeroBits = Just (seen .|. bits)
      | otherwise = Nothing

    -- The REX byte is stored verbatim in the low byte of the
    -- 'PrefixCode'. A valid REX byte has bits 3..0 = W,R,X,B and bit 6
    -- = 1, so this write sets the REX sub-bits plus 'rexSeenBit'.
    addRex byte
      | seen .&. rexMask /= zeroBits = Nothing
      | otherwise = Just (seen .|. PrefixCode (fromIntegral @Word8 @Word16 byte))
{-# INLINE addPrefixCode #-}

-- Segment observation codes: three-bit values assigned so that every
-- single-segment observation is a subset of 'segMask' (for defs allowing
-- any segment) and only the DS/notrack observation is a subset of
-- 'segDsBit' alone.
segEs, segCs, segSs, segDs, segFs, segGs :: PrefixCode
segEs = segBit0 -- 0b001
segCs = segBit1 -- 0b010
segSs = segBit0 .|. segBit1 -- 0b011
segDs = segDsBit -- 0b100
segFs = segBit0 .|. segDsBit -- 0b101
segGs = segBit1 .|. segDsBit -- 0b110

-- | Check whether an observed 'Seen' is compatible with a def's allowed
-- prefix set. The caller passes the def's effective-allowed mask (i.e.
-- 'Flexdis86.OpTable.defPrefix', which already has the required-prefix
-- bit OR'd in at parse time), so 'containedIn' can be a single bitwise
-- test.
checkAllowed :: Seen -> VEX.VEX -> Allowed -> Required -> Bool
checkAllowed (Seen seen) vex allowed req =
  seen `containedIn` allowedCode allowed && requiredSeen && rexVexDisjoint
  where
    -- A def with a required prefix only validates when that bit was
    -- actually observed.
    requiredSeen =
      let rc = requiredCode req
      in rc == zeroBits || seen .&. rc /= zeroBits

    -- Having both REX and VEX prefixes is #UD.
    rexVexDisjoint = seen .&. rexSeenBit == zeroBits || not (VEX.hasVex vex)
{-# INLINE checkAllowed #-}

