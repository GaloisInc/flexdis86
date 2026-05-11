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
  , emptySeen
  , addPrefix
  , materializePrefixes
  , checkAllowed
  ) where

import           Control.Exception (assert)
import qualified Control.DeepSeq as DS
import           Data.Binary (Binary)
import           Data.Bits (Bits, (.&.), (.|.), shiftL, zeroBits, complement, unsafeShiftR)
import           Data.Coerce (coerce)
import           Data.Word (Word8, Word16, Word64)

import           Flexdis86.Prefixes
import           Flexdis86.Prefixes.Allowed
import           Flexdis86.Prefixes.Code
import           Flexdis86.Prefixes.Required

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

-- | A branchless lookup table mapping the 3-bit segment code to the
-- corresponding prefix byte. Each byte slot of the 'Word64' is indexed by
-- the seg code (1..6). Slot 0 (no seg override) holds @0x00@; unused
-- slot 7 also holds @0x00@.
--
-- To materialize: take the segment-code field of a 'PrefixCode' (at bit
-- positions [10..12]), multiply by 8 to turn it into a bit offset, shift
-- the table down by that amount, and mask to a byte. The multiply-by-8
-- can be fused with the shift-from-bit-10: the raw shared bits already
-- encode @segCode << 10@, so shifting right by 7 yields @segCode << 3 =
-- segCode * 8@.
segByteTable :: Word64
segByteTable =
      fromIntegral @Word8 @Word64 esPrefixByte `shiftL` 8
  .|. fromIntegral @Word8 @Word64 csPrefixByte `shiftL` 16
  .|. fromIntegral @Word8 @Word64 ssPrefixByte `shiftL` 24
  .|. fromIntegral @Word8 @Word64 dsPrefixByte `shiftL` 32
  .|. fromIntegral @Word8 @Word64 fsPrefixByte `shiftL` 40
  .|. fromIntegral @Word8 @Word64 gsPrefixByte `shiftL` 48

-- | Recover the REX byte stored in the low byte of a 'PrefixCode'.
-- Returns 0 if no REX prefix was observed.
seenRexByte :: PrefixCode -> Word8
seenRexByte (PrefixCode w) = fromIntegral @Word16 @Word8 w .&. 0x4F
{-# INLINE seenRexByte #-}

-- | Look up the prefix byte corresponding to a 3-bit segment code,
-- given the masked seg-code bits in their raw position (bits [10..12])
-- of a 'PrefixCode'. Shift-right-by-7 turns @code << 10@ into
-- @code * 8@.
--
-- Uses 'unsafeShiftR' to avoid a bounds check on the 'Word64' shift:
-- the seg code is always in [0, 7] so the shift amount is always in
-- [0, 56], well below the 64-bit limit.
lookupSegByte :: PrefixCode -> Word8
lookupSegByte pc@(PrefixCode w_) =
  let w = assert (pc .&. complement segMask == zeroBits) w_
      shiftAmt_ = fromIntegral @Word16 @Int (w `unsafeShiftR` 7)
      shiftAmt = assert (shiftAmt_ >= 0 && shiftAmt_ < 64) shiftAmt_
  in fromIntegral @Word64 @Word8
       (segByteTable `unsafeShiftR` shiftAmt)
{-# INLINE lookupSegByte #-}

-- | Check whether an observed 'Seen' is compatible with a def's allowed
-- prefix set. The def's 'Required' bits are temporarily OR'd into the
-- effective allowed mask - SSE instructions encode required-prefix bytes
-- that are bitwise identical to legacy-prefix bytes (e.g. @0xF3@ is both
-- a required prefix for @endbr32@ and the wire byte for @REP@), so such
-- bytes are permitted even when the corresponding legacy-prefix bit is not
-- in @allowed@.
checkAllowed :: Seen -> Maybe VEX -> Allowed -> Required -> Bool
checkAllowed (Seen seen) mbVex allowed req =
  seen `containedIn` effectiveAllowed && requiredSeen && rexVexDisjoint
  where
    -- The required-prefix bit(s) are also legacy-prefix observations;
    -- admit them in the effective allowed mask so 'containedIn' does not
    -- reject the instruction.
    effectiveAllowed = allowedCode allowed .|. requiredCode req

    -- A def with a required prefix only validates when that bit was
    -- actually observed.
    requiredSeen =
      let rc = requiredCode req
      in rc == zeroBits || seen .&. rc /= zeroBits

    -- Having both REX and VEX prefixes is #UD.
    rexVexDisjoint = seen .&. rexSeenBit == zeroBits || case mbVex of
      Nothing -> True
      Just _  -> False
{-# INLINE checkAllowed #-}

-- | Reconstruct a 'Prefixes' value from the observed prefixes, the VEX
-- prefix (if any), and the def's allowed prefix set plus required prefix
-- byte (if any).
--
-- A required-prefix observation (e.g. @0xF3@ for @endbr32@) does not
-- contribute to its legacy-prefix role: the corresponding bit is cleared
-- from the accumulator before reading off 'Prefixes' fields.
--
-- Other context-dependent cases:
--
--  * REP (0xF3): materializes as 'RepZPrefix' if the def's allowed set
--    includes @repz@ ('repzSemanticBit'), otherwise 'RepPrefix'.
--
--  * Segment/notrack (0x3E): materializes as a notrack flag if the def's
--    allowed set has @notrack@ semantics ('notrackSemanticBit') without
--    allowing any segment override; otherwise it is a DS segment override.
materializePrefixes :: Seen -> Maybe VEX -> Allowed -> Required -> Prefixes
materializePrefixes (Seen seen) mbVex allowed req = Prefixes
  { _prLockPrefix = lockPrefix
  , _prSP = SegmentPrefix segByte
  , _prREX = REX rexByte
  , _prVEX = mbVex
  , _prASO = legacy .&. asoBit /= zeroBits
  , _prOSO = legacy .&. osoBit /= zeroBits
  , _prNoTrack = asNotrack
  }
  where
    allowedBits = allowedCode allowed

    -- Clear the required-prefix bit(s) so they don't register as
    -- legacy-prefix observations.
    legacy = seen .&. complement (requiredCode req)

    lockPrefix
      | legacy .&. lockBit /= zeroBits = LockPrefix
      | legacy .&. repNZBit /= zeroBits = RepNZPrefix
      | legacy .&. repBit /= zeroBits =
          if allowedBits .&. allowedCode repzSemanticBit /= zeroBits
            then RepZPrefix
            else RepPrefix
      | otherwise = NoLockPrefix

    segBits = legacy .&. segMask
    asNotrack =
      segBits == segDs
        && allowedBits .&. allowedCode notrackSemanticBit /= zeroBits
        && allowedBits .&. (segBit0 .|. segBit1) == zeroBits
    -- Branchless materialization via a byte-indexed Word64 lookup
    -- table; see 'segByteTable'. Zero seg-code slots and the notrack
    -- case both read 0x00 from the table.
    segByte
      | asNotrack = 0x00
      | otherwise = lookupSegByte segBits

    rexByte = seenRexByte legacy
{-# INLINE materializePrefixes #-}

