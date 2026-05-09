{- |
Module      : Flexdis86.PrefixAccum
Copyright   : (c) Galois, Inc, 2026

A compact bitwise accumulator for legacy prefix bytes plus REX.
-}
{-# LANGUAGE BangPatterns #-}
module Flexdis86.PrefixAccum
  ( PrefixCode(..)
  , emptyPrefixCode
  , addPrefix
  , materializePrefixes
  , checkPrefixCode
  ) where

import Data.Bits
import Data.Word

import Flexdis86.Prefixes
import Flexdis86.PrefixSet

-- | Compact prefix accumulator.
--
-- @pcLegacy@ layout:
--
-- @
--   [1:0]  lock/rep group:  0=none, 1=LOCK, 2=REPNZ, 3=REP/REPZ
--   [4:2]  segment group:   0=none, 1=ES, 2=CS, 3=SS, 4=DS/notrack, 5=FS, 6=GS
--   [5]    OSO (0x66) seen
--   [6]    ASO (0x67) seen
--   [7]    (reserved)
-- @
--
-- @pcRex@ holds the raw REX prefix byte (0x40..0x4F), or 0 if no REX prefix
-- was encountered.
data PrefixCode = PrefixCode
  { pcLegacy :: {-# UNPACK #-} !Word8
  -- | Raw REX prefix byte. FIXME: we could also decode this into its
  -- constituent w\/r\/x\/b bits here rather than leaving it raw.
  , pcRex :: {-# UNPACK #-} !Word8
  }

emptyPrefixCode :: PrefixCode
emptyPrefixCode = PrefixCode 0 0

-- Bit-field constants for 'pcLegacy'.
lockRepMask, segMask, osoBit, asoBit :: Word8
lockRepMask = 0x03
segMask = 0x1C
osoBit = 0x20
asoBit = 0x40

-- | Bit position of the segment-code field within 'pcLegacy'.
segShift :: Int
segShift = 2

-- Lock/rep group codes (bottom 2 bits of 'pcLegacy').
noLockRepCode, lockCode, repNZCode, repCode :: Word8
noLockRepCode = 0
lockCode = 1
repNZCode = 2
repCode = 3

-- Segment-code values for the 3-bit field at bits [4:2] of 'pcLegacy'.
noSegCode, esCode, csCode, ssCode, dsCode, fsCode, gsCode :: Word8
noSegCode = 0
esCode = 1
csCode = 2
ssCode = 3
dsCode = 4 -- also used for the notrack prefix byte 0x3E
fsCode = 5
gsCode = 6

-- Individual REX bit masks (bottom nibble of the REX byte).
rexWBit, rexRBit, rexXBit, rexBBit :: Word8
rexWBit = 0x08
rexRBit = 0x04
rexXBit = 0x02
rexBBit = 0x01

-- | Mask of all four REX bits (w/r/x/b).
rexBitsMask :: Word8
rexBitsMask = rexWBit .|. rexRBit .|. rexXBit .|. rexBBit

-- | Fold a legacy prefix byte into the accumulator. Returns 'Nothing' if
-- the byte is not a legacy prefix or if a prefix group is already
-- occupied (duplicate prefixes from the same group are rejected).
addPrefix :: PrefixCode -> Word8 -> Maybe PrefixCode
addPrefix (PrefixCode leg rex) b
  | b == lockPrefixByte = lr lockCode
  | b == repNZPrefixByte = lr repNZCode
  | b == repPrefixByte = lr repCode
  | b == esPrefixByte = seg esCode
  | b == csPrefixByte = seg csCode
  | b == ssPrefixByte = seg ssCode
  | b == dsPrefixByte = seg dsCode
  | b == fsPrefixByte = seg fsCode
  | b == gsPrefixByte = seg gsCode
  | b == operandSizeOverrideByte = setBit' osoBit
  | b == addrSizeOverrideByte = setBit' asoBit
  | isRexPrefixByte b = if rex /= 0 then Nothing else Just (PrefixCode leg b)
  | otherwise = Nothing
  where
    lr code
      | leg .&. lockRepMask /= 0 = Nothing
      | otherwise = Just (PrefixCode (leg .|. code) rex)
    seg code
      | leg .&. segMask /= 0 = Nothing
      | otherwise = Just (PrefixCode ((leg .&. complement segMask)
                                       .|. (code `shiftL` segShift)) rex)
    setBit' bit_
      | leg .&. bit_ /= 0 = Nothing
      | otherwise = Just (PrefixCode (leg .|. bit_) rex)
{-# INLINE addPrefix #-}

-- | Decode the segment field into the prefix byte stored in 'SegmentPrefix'.
-- Code 0 maps to 'no_seg_prefix' (byte 0x00).
segCodeToByte :: Word8 -> Word8
segCodeToByte c
  | c == esCode = esPrefixByte
  | c == csCode = csPrefixByte
  | c == ssCode = ssPrefixByte
  | c == dsCode = dsPrefixByte
  | c == fsCode = fsPrefixByte
  | c == gsCode = gsPrefixByte
  | otherwise = 0x00
{-# INLINE segCodeToByte #-}

-- | Does a def's allowed prefix set include 'pfxNotrack' but not 'pfxSeg'? If
-- both are allowed, 'pfxSeg' wins. No def in @optable.xml@ allows both.
treatsDs3eAsNotrack :: PrefixSet -> Bool
treatsDs3eAsNotrack allowed =
  hasPfx pfxNotrack allowed && not (hasPfx pfxSeg allowed)
{-# INLINE treatsDs3eAsNotrack #-}

-- | Reconstruct a 'Prefixes' value from the compact accumulator, the VEX prefix
-- (if any), and the def's allowed prefix set plus required prefix byte (if
-- any).
--
-- The required-prefix byte (when present) is always one of @0x66@, @0xF2@,
-- or @0xF3@. These bytes also serve double duty as legacy prefixes (OSO,
-- REPNZ, REP), so when such a byte appears in the stream and matches the def's
-- required prefix, we attribute it to the required-prefix role rather than
-- to the legacy prefix role.
--
-- Other context-dependent cases:
--
--  * REP (0xF3): becomes 'RepZPrefix' if the def allows @repz@, otherwise
--    'RepPrefix'. (A single byte 0xF3 can serve both roles depending on
--    the instruction.)
--
--  * Segment/notrack (0x3E): becomes a notrack flag for defs that allow
--    @notrack@ but not any segment override; otherwise it is a DS segment
--    override.
materializePrefixes :: PrefixCode -> Maybe VEX -> PrefixSet -> Maybe Word8 -> Prefixes
materializePrefixes (PrefixCode leg rex) mbVex allowed mbReq = Prefixes
  { _prLockPrefix = lp
  , _prSP = SegmentPrefix segByte
  , _prREX = REX rex
  , _prVEX = mbVex
  , _prASO = leg .&. asoBit /= 0
  , _prOSO = oso
  , _prNoTrack = asNotrack
  }
  where
    lrCode = leg .&. lockRepMask
    -- A lock/rep byte that matches the required prefix is not a lock/rep.
    reqEatsLR = case mbReq of
      Just b | b == repNZPrefixByte -> lrCode == repNZCode
             | b == repPrefixByte -> lrCode == repCode
      _ -> False
    lp | reqEatsLR = NoLockPrefix
       | lrCode == lockCode = LockPrefix
       | lrCode == repNZCode = RepNZPrefix
       | lrCode == repCode && hasPfx pfxRepz allowed = RepZPrefix
       | lrCode == repCode = RepPrefix
       | otherwise = NoLockPrefix

    -- Likewise for OSO (0x66) when that is the required prefix.
    reqEatsOso = mbReq == Just operandSizeOverrideByte
    oso = leg .&. osoBit /= 0 && not reqEatsOso

    segCode = (leg `shiftR` segShift) .&. (segMask `shiftR` segShift)
    isDsByte = segCode == dsCode
    asNotrack = isDsByte && treatsDs3eAsNotrack allowed
    segByte = if asNotrack then 0x00 else segCodeToByte segCode
{-# INLINE materializePrefixes #-}

-- | Validate a 'PrefixCode' and optional VEX against a def's allowed prefix
-- set and required prefix byte. Mirrors the old per-byte 'HashMap'
-- lookups in 'validatePrefixBytes' but without any allocation.
checkPrefixCode
  :: PrefixCode
  -> Maybe VEX
  -> PrefixSet -- ^ def's allowed legacy prefixes
  -> Maybe Word8 -- ^ def's required prefix byte, if any
  -> Bool
checkPrefixCode (PrefixCode leg rex) mbVex allowed mbReq =
  lockRepOK && segOK && osoOK && asoOK && rexOK && rexVexDisjoint && reqSeen
  where
    lockRepCode = leg .&. lockRepMask
    lockRepOK
      | lockRepCode == noLockRepCode = True
      | lockRepCode == lockCode = hasPfx pfxLock allowed
      | lockRepCode == repNZCode = hasPfx pfxRepnz allowed || mbReq == Just repNZPrefixByte
      | lockRepCode == repCode = hasPfx pfxRep allowed || hasPfx pfxRepz allowed || mbReq == Just repPrefixByte
      | otherwise = False

    segCode = (leg `shiftR` segShift) .&. (segMask `shiftR` segShift)
    segOK
      | segCode == noSegCode = True
      | segCode == dsCode = hasPfx pfxSeg allowed || hasPfx pfxNotrack allowed
      | otherwise = hasPfx pfxSeg allowed

    osoOK = leg .&. osoBit == 0 || hasPfx pfxOso allowed || mbReq == Just operandSizeOverrideByte
    asoOK = leg .&. asoBit == 0 || hasPfx pfxAso allowed

    rexOK
      | rex == 0 = True
      | otherwise = rex .&. rexBitsMask .&. complement allowedRexBits == 0
    allowedRexBits =
      (if hasPfx pfxRexw allowed then rexWBit else 0)
      .|. (if hasPfx pfxRexr allowed then rexRBit else 0)
      .|. (if hasPfx pfxRexx allowed then rexXBit else 0)
      .|. (if hasPfx pfxRexb allowed then rexBBit else 0)

    rexVexDisjoint = rex == 0 || case mbVex of
      Nothing -> True
      Just _ -> False

    reqSeen = case mbReq of
      Nothing -> True
      Just b
        | b == 0x66 -> leg .&. osoBit /= 0
        | b == 0xF2 -> lockRepCode == 2
        | b == 0xF3 -> lockRepCode == 3
        | otherwise -> False
{-# INLINE checkPrefixCode #-}
