{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2014-2026
Maintainer  : jhendrix@galois.com

Prefix information carried on a decoded 'InstructionInstance'.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Flexdis86.Prefixes
  ( -- * Prefixes
    Prefixes
  , noPrefixes
  , mkPrefixes
  , prLockPrefix
  , prSP
  , prREX
  , prVEX
  , prASO
  , prOSO
  , prNoTrack
  , prAddrSize
  , clearPrOSO
  , clearPrLockPrefix
  , PrefixFields(..)
  , MkPrefixesError(..)
  , mkPrefixesFromFields
    -- * SegmentPrefix
  , SegmentPrefix(..)
  , no_seg_prefix
  , setDefault
    -- * LockPrefix
  , LockPrefix(..)
  , ppLockPrefix
    -- * Prefix byte constants
  , module Flexdis86.Prefixes.Bytes
  ) where

import           Control.Exception (assert)
import           Control.Monad (when)
import qualified Control.DeepSeq as DS
import qualified Data.Bits as B
import           Data.Bits ((.&.), (.|.), complement, unsafeShiftR, zeroBits)
import           Data.Word (Word8, Word16, Word64)
import           Numeric (showHex)
import qualified Prettyprinter as PP

import           Flexdis86.Prefixes.Allowed
                   ( Allowed, allowedCode, notrackSemanticBit, repzSemanticBit )
import           Flexdis86.Prefixes.Bytes
import           Flexdis86.Prefixes.Code
import           Flexdis86.Prefixes.REX (REX(..))
import           Flexdis86.Prefixes.Required (Required, requiredCode)
import           Flexdis86.Prefixes.Seen (Seen, seenCode)
import qualified Flexdis86.VEX.Seen as VEX
import           Flexdis86.Segment
import           Flexdis86.Sizes

------------------------------------------------------------------------
-- SegmentPrefix

-- | Includes segment prefix and branch override hints.
newtype SegmentPrefix = SegmentPrefix { unwrapSegmentPrefix :: Word8 }
  deriving (Eq, Show)

instance DS.NFData SegmentPrefix where
  rnf !_ = ()

no_seg_prefix :: SegmentPrefix
no_seg_prefix = SegmentPrefix 0

setDefault :: SegmentPrefix -> Segment -> Segment
setDefault (SegmentPrefix 0) s = s
setDefault (SegmentPrefix 0x26) _ = ES
setDefault (SegmentPrefix 0x2e) _ = CS
setDefault (SegmentPrefix 0x36) _ = SS
setDefault (SegmentPrefix 0x3e) _ = DS
setDefault (SegmentPrefix 0x64) _ = FS
setDefault (SegmentPrefix 0x65) _ = GS
setDefault (SegmentPrefix w) _ = error $ "Unexpected segment prefix: " ++ showHex w ""

-----------------------------------------------------------------------
-- LockPrefix

data LockPrefix
   = NoLockPrefix
   | LockPrefix
   | RepPrefix
   | RepZPrefix
   | RepNZPrefix
  deriving (Eq, Show)

instance DS.NFData LockPrefix where
  rnf !_ = ()

ppLockPrefix :: LockPrefix -> PP.Doc a
ppLockPrefix NoLockPrefix = ""
ppLockPrefix LockPrefix = "lock"
ppLockPrefix RepPrefix  = "rep"
ppLockPrefix RepZPrefix = "repz"
ppLockPrefix RepNZPrefix = "repnz"

-----------------------------------------------------------------------
-- Prefixes

-- | Merged prefix state for a decoded instruction. Both fields
-- 'UNPACK' into 'Prefixes'; 'Prefixes' in turn 'UNPACK's into the
-- 'InstructionInstance' record.
--
-- 'prfCode' is the observed 'Seen' bits (with any required-prefix bit
-- cleared) ORed with the two semantic bits from the def's 'Allowed'
-- ('repzSemanticBit', 'notrackSemanticBit'). This way read-off of
-- 'prLockPrefix' and 'prNoTrack' doesn't need 'Allowed' threaded
-- through.
data Prefixes = Prefixes
  { prfCode :: {-# UNPACK #-} !PrefixCode
  , prfVex  :: {-# UNPACK #-} !VEX.VEX
  }
  deriving (Eq, Show)

instance DS.NFData Prefixes where
  rnf (Prefixes _ _) = ()

-- | 'Prefixes' with no bits set and no VEX observed. Used by the
-- assembler to seed a fresh instruction.
noPrefixes :: Prefixes
noPrefixes = Prefixes emptyPrefixCode VEX.noVex

prVEX :: Prefixes -> VEX.VEX
prVEX = prfVex
{-# INLINE prVEX #-}

-- | Recover the observed REX byte, or @0x00@ if no REX prefix was
-- observed.
prREX :: Prefixes -> REX
prREX (Prefixes pc _) =
  -- Bits 4, 5, 7 of the low byte carry non-REX observations
  -- (lock/repNZ/rep); mask to 'rexMask' to extract just the REX
  -- sub-bits plus 'rexSeenBit'. Invariant: if any REX sub-bit is set,
  -- 'rexSeenBit' must also be set, so the reconstructed byte has the
  -- REX pattern @0b0100WRXB@ (i.e. 'isRexPrefixByte' holds).
  let PrefixCode m = pc .&. rexMask
      b = fromIntegral @Word16 @Word8 m
  in assert (b == 0 || isRexPrefixByte b) (REX b)
{-# INLINE prREX #-}

prOSO :: Prefixes -> Bool
prOSO (Prefixes c _) = c .&. osoBit /= zeroBits
{-# INLINE prOSO #-}

prASO :: Prefixes -> Bool
prASO (Prefixes c _) = c .&. asoBit /= zeroBits
{-# INLINE prASO #-}

-- | Resolve the lock/rep group. 'RepZPrefix' vs 'RepPrefix' is
-- disambiguated by 'repzSemanticBit' (pre-merged into 'prfCode').
prLockPrefix :: Prefixes -> LockPrefix
prLockPrefix (Prefixes c _) =
  -- 'addPrefix' and 'mkPrefixesFromFields' both reject multiple
  -- lock-group observations, so at most one of 'lockBit', 'repNZBit',
  -- 'repBit' is set here.
  assert (atMostOneBitSet (c .&. lockRepGroup)) $
  if | c .&. lockBit /= zeroBits -> LockPrefix
     | c .&. repNZBit /= zeroBits -> RepNZPrefix
     | c .&. repBit /= zeroBits ->
         if c .&. allowedCode repzSemanticBit /= zeroBits
           then RepZPrefix else RepPrefix
     | otherwise -> NoLockPrefix
  where
    lockRepGroup = lockBit .|. repNZBit .|. repBit
{-# INLINE prLockPrefix #-}

-- | True iff at most one bit is set. @x .&. (x - 1) == 0@ is the
-- standard \"power of two or zero\" idiom.
atMostOneBitSet :: PrefixCode -> Bool
atMostOneBitSet (PrefixCode w) = w .&. (w - 1) == 0
{-# INLINE atMostOneBitSet #-}

-- | Is this a @notrack@ prefix observation (as opposed to a DS segment
-- override)?
prNoTrack :: Prefixes -> Bool
prNoTrack (Prefixes c _) =
  c .&. segMask == segDsBit
    && c .&. allowedCode notrackSemanticBit /= zeroBits
{-# INLINE prNoTrack #-}

-- | The observed segment-override byte, or @0x00@ if none (or if the
-- observation is actually a @notrack@).
prSP :: Prefixes -> SegmentPrefix
prSP pfx@(Prefixes c _)
  -- When 'prNoTrack' holds, 'mkPrefixes' guarantees the seg-code field
  -- is exactly 'segDsBit' - i.e. no other seg-override byte was
  -- observed alongside the notrack byte.
  | prNoTrack pfx = assert (c .&. segMask == segDsBit) no_seg_prefix
  | otherwise = SegmentPrefix (lookupSegByte (c .&. segMask))
{-# INLINE prSP #-}

prAddrSize :: Prefixes -> SizeConstraint
prAddrSize pfx
  | prASO pfx = Size32
  | otherwise = Size64
{-# INLINE prAddrSize #-}

-- | Clear the OSO bit. Used by 'applyNopFamilyFixups' when a NOP with
-- OSO is actually an XCHG.
clearPrOSO :: Prefixes -> Prefixes
clearPrOSO (Prefixes c v) = Prefixes (c .&. complement osoBit) v
{-# INLINE clearPrOSO #-}

-- | Clear the lock\/rep group. Used by 'applyNopFamilyFixups' when a
-- NOP with REP is actually a PAUSE.
clearPrLockPrefix :: Prefixes -> Prefixes
clearPrLockPrefix (Prefixes c v) = Prefixes (c .&. complement lockRepMask) v
  where
    lockRepMask = lockBit .|. repNZBit .|. repBit
{-# INLINE clearPrLockPrefix #-}

-- | Merge a 'Seen' with a def's 'Allowed' and 'Required' plus the
-- observed VEX into a 'Prefixes'. The result's 'prfCode' is:
--
--   * The observed bits from 'Seen' with the 'Required' bit(s) cleared
--     (so e.g. the mandatory 0xF3 byte of @endbr32@ doesn't register as
--     a legacy REP prefix).
--
--   * ORed with the two semantic bits from 'Allowed' ('repzSemanticBit',
--     'notrackSemanticBit'), so read-off of 'prLockPrefix' and
--     'prNoTrack' doesn't need 'Allowed' threaded through.
--
-- 'notrack' and @seg@ are mutually exclusive on any given def -
-- 'Flexdis86.Prefixes.Allowed.allowedFromNames' rejects defs that would
-- combine them - so we can copy 'notrackSemanticBit' through without
-- ambiguity.
mkPrefixes :: Seen -> VEX.VEX -> Allowed -> Required -> Prefixes
mkPrefixes seen vex allowed req =
  assert notrackExcludesSeg $
  Prefixes (legacy .|. semanticBits) vex
  where
    -- Clear the required-prefix bit(s) so they don't register as
    -- legacy-prefix observations.
    legacy = seenCode seen .&. complement (requiredCode req)
    semanticMask =
      allowedCode repzSemanticBit .|. allowedCode notrackSemanticBit
    semanticBits = allowedCode allowed .&. semanticMask

    notrackExcludesSeg =
      allowedCode allowed .&. allowedCode notrackSemanticBit == zeroBits
        || allowedCode allowed .&. (segBit0 .|. segBit1) == zeroBits
{-# INLINE mkPrefixes #-}

-- | Input to 'mkPrefixesFromFields'.
data PrefixFields = PrefixFields
  { pfLockPrefix :: !LockPrefix
  , pfSP :: !SegmentPrefix
  , pfREX :: !REX
  , pfVEX :: !VEX.VEX
  , pfASO :: !Bool
  , pfOSO :: !Bool
  , pfNoTrack :: !Bool
  }

-- | What can go wrong building a 'Prefixes' from a 'PrefixFields'.
data MkPrefixesError
  = -- | The 'pfREX' byte is neither @0x00@ nor of the form @0b0100WRXB@.
    InvalidREXByte !Word8
  | -- | 'pfNoTrack' is set and 'pfSP' is a real segment override; the
    -- two assign conflicting meanings to the 0x3E byte.
    NotrackAndSegOverride
  | -- | 'pfSP' is not a recognized segment-override byte.
    InvalidSegmentPrefix !Word8
  deriving (Eq, Show)

-- | Build a 'Prefixes' from fielded inputs. Used by the assembler,
-- which synthesizes prefixes per-field rather than decoding them from
-- a byte stream. The decoder uses 'mkPrefixes' instead.
mkPrefixesFromFields :: PrefixFields -> Either MkPrefixesError Prefixes
mkPrefixesFromFields pf = do
  -- Check: pfREX is either 0 or has bit 6 set (the @0x40@ of a valid
  -- @0b0100WRXB@ byte). If any sub-bit is set without bit 6, the caller
  -- has handed us something that doesn't mean REX.
  when (r /= 0 && r .&. 0x40 == 0) $
    Left (InvalidREXByte r)
  -- Check: pfNoTrack and pfSP are mutually exclusive - they conflict on
  -- the 0x3E byte. Reject instead of silently dropping one.
  when (pfNoTrack pf && sb /= 0) $
    Left NotrackAndSegOverride
  segBits <- segBitsFrom sb
  pure $ Prefixes
    ( PrefixCode (fromIntegral @Word8 @Word16 r)
    .|. lockBits (pfLockPrefix pf)
    .|. segBits
    .|. (if pfASO pf then asoBit else zeroBits)
    .|. (if pfOSO pf then osoBit else zeroBits)
    .|. notrackBits
    )
    (pfVEX pf)
  where
    REX r = pfREX pf
    SegmentPrefix sb = pfSP pf

    -- Lock/rep: set the observed bit plus the repz semantic bit if the
    -- caller specified 'RepZPrefix'.
    lockBits NoLockPrefix = zeroBits
    lockBits LockPrefix = lockBit
    lockBits RepNZPrefix = repNZBit
    lockBits RepPrefix = repBit
    lockBits RepZPrefix = repBit .|. allowedCode repzSemanticBit

    -- notrack: set the DS seg bit plus the notrack semantic bit. The DS
    -- segment override has the same representation minus the semantic
    -- bit, so the two cases remain distinguishable by 'prNoTrack'.
    notrackBits
      | pfNoTrack pf = segDsBit .|. allowedCode notrackSemanticBit
      | otherwise = zeroBits

    -- Segment override: translate the raw byte back to the 3-bit seg
    -- code.
    segBitsFrom b
      | b == 0 = Right zeroBits
      | b == esPrefixByte = Right segBit0
      | b == csPrefixByte = Right segBit1
      | b == ssPrefixByte = Right (segBit0 .|. segBit1)
      | b == dsPrefixByte = Right segDsBit
      | b == fsPrefixByte = Right (segBit0 .|. segDsBit)
      | b == gsPrefixByte = Right (segBit1 .|. segDsBit)
      | otherwise = Left (InvalidSegmentPrefix b)

------------------------------------------------------------------------
-- Internal

-- | Branchless segment-code -> prefix-byte lookup. See
-- 'Flexdis86.Prefixes.Seen.segByteTable' for the full explanation; this
-- is a copy of the same table, keyed on bits [10..12] of a
-- 'PrefixCode'.
lookupSegByte :: PrefixCode -> Word8
lookupSegByte (PrefixCode w) =
  let shiftAmt = fromIntegral @Word16 @Int (w `unsafeShiftR` 7)
  in fromIntegral @Word64 @Word8 (segByteTable `unsafeShiftR` shiftAmt)
{-# INLINE lookupSegByte #-}

segByteTable :: Word64
segByteTable =
      fromIntegral @Word8 @Word64 esPrefixByte `B.shiftL` 8
  .|. fromIntegral @Word8 @Word64 csPrefixByte `B.shiftL` 16
  .|. fromIntegral @Word8 @Word64 ssPrefixByte `B.shiftL` 24
  .|. fromIntegral @Word8 @Word64 dsPrefixByte `B.shiftL` 32
  .|. fromIntegral @Word8 @Word64 fsPrefixByte `B.shiftL` 40
  .|. fromIntegral @Word8 @Word64 gsPrefixByte `B.shiftL` 48
