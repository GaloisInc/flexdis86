{- |
Module      : Flexdis86.Prefixes.Allowed
Copyright   : (c) Galois, Inc, 2026

The set of prefixes a 'Def' permits, represented as a
'Flexdis86.Prefixes.Code.PrefixCode'. Each @pfxX@ flag is a 'PrefixCode'
with the corresponding validation bits set, so @allowed@ can be built up
by ORing the flags together:

@
  allowed = 'pfxLock' .|. 'pfxOso' .|. 'pfxRexw' .|. 'pfxSeg'
@

Validation against a 'Flexdis86.Prefixes.Seen.Seen' is
'Flexdis86.Prefixes.Code.containedIn'.

An 'Allowed' value uses the shared-bit layout from
"Flexdis86.Prefixes.Code" as is, and additionally uses the two
validation-excluded bits for semantic flags consulted at materialization
time:

@
  [13]  'repzSemanticBit'     -- a 'repBit' observation becomes 'RepZPrefix'
  [14]  'notrackSemanticBit'  -- a 'segDsBit' observation becomes a notrack flag
@
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Flexdis86.Prefixes.Allowed
  ( Allowed
  , allowedCode
  , noPrefixes
  , pfxAso
  , pfxNotrack
  , pfxOso
  , pfxRep
  , pfxRepnz
  , pfxRepz
  , pfxRexb
  , pfxRexr
  , pfxRexx
  , pfxRexw
  , pfxLock
  , pfxSeg
  , hasPfx
  , allowedOrCode
  , allowedFromNames
    -- * Semantic bits used at materialization
  , repzSemanticBit
  , notrackSemanticBit
  ) where

import qualified Control.DeepSeq as DS
import           Data.Binary (Binary)
import           Data.Bits (Bits, (.&.), (.|.), shiftL, zeroBits)

import           Flexdis86.Prefixes.Code

-- | The set of prefixes a 'Def' permits. Wraps a 'PrefixCode'; see
-- "Flexdis86.Prefixes.Code" for the shared bit layout. 'Allowed'
-- additionally uses the two validation-excluded bits for semantic flags
-- consulted at materialization time:
--
-- @
--   [13]  'repzSemanticBit'     -- a 'repBit' observation becomes 'RepZPrefix'
--   [14]  'notrackSemanticBit'  -- a 'segDsBit' observation becomes a notrack flag
-- @
newtype Allowed = Allowed { allowedCode :: PrefixCode }
  deriving (Bits, Binary, DS.NFData, Eq, Ord, Show)

-- | An empty 'Allowed' set (no prefixes permitted).
noPrefixes :: Allowed
noPrefixes = Allowed emptyPrefixCode

pfxAso, pfxNotrack, pfxOso, pfxRep, pfxRepnz, pfxRepz :: Allowed
pfxRexb, pfxRexr, pfxRexx, pfxRexw, pfxLock, pfxSeg   :: Allowed

pfxLock    = Allowed lockBit
pfxRepnz   = Allowed repNZBit
-- | Either @rep@ or @repz@ observation of 0xF3 is permitted. 'pfxRepz'
-- additionally sets 'repzSemanticBit' so materialization yields
-- 'RepZPrefix' rather than 'RepPrefix'.
pfxRep     = Allowed repBit
pfxRepz    = Allowed repBit .|. repzSemanticBit
pfxOso     = Allowed osoBit
pfxAso     = Allowed asoBit
-- | Any segment override is permitted: all three segment-code bits are
-- set in the mask, so any observed seg code is a subset.
pfxSeg     = Allowed segMask
-- | Only the 0x3E prefix byte is permitted, and it materializes as a
-- notrack flag. Does not permit any other segment override.
pfxNotrack = Allowed segDsBit .|. notrackSemanticBit

-- Any REX allowance also implicitly allows bare @0x40@, so we always OR
-- in 'rexSeenBit'.
pfxRexb    = Allowed (rexBBit .|. rexSeenBit)
pfxRexx    = Allowed (rexXBit .|. rexSeenBit)
pfxRexr    = Allowed (rexRBit .|. rexSeenBit)
pfxRexw    = Allowed (rexWBit .|. rexSeenBit)

-- | An 'Allowed' whose def permits @repz@, so a 'repBit' observation
-- materializes as 'RepZPrefix' rather than 'RepPrefix'. Bit 13 - outside
-- 'sharedBitsMask' so it does not participate in validation.
repzSemanticBit :: Allowed
repzSemanticBit = Allowed (PrefixCode (1 `shiftL` 13))

-- | An 'Allowed' whose def permits @notrack@, so an observation of 'segDsBit'
-- without any other segment bits materializes as a @notrack@ flag rather than
-- a DS segment override. Bit 14 - outside 'sharedBitsMask' so it does not
-- participate in validation.
notrackSemanticBit :: Allowed
notrackSemanticBit = Allowed (PrefixCode (1 `shiftL` 14))

-- | Test whether a particular prefix flag is set.
hasPfx :: Allowed -- ^ Flag to test (e.g. 'pfxRep')
       -> Allowed -- ^ The set to test against
       -> Bool
hasPfx flag ps = ps .&. flag /= zeroBits
{-# INLINE hasPfx #-}

-- | OR a raw 'PrefixCode' into an 'Allowed'. Intended for
-- "Flexdis86.Prefixes.Required" to fold the required-prefix bit into
-- the effective-allowed mask without exposing the 'Allowed' constructor.
allowedOrCode :: Allowed -> PrefixCode -> Allowed
allowedOrCode (Allowed a) c = Allowed (a .|. c)
{-# INLINE allowedOrCode #-}

-- | Build an 'Allowed' set from the prefix name strings used in
-- @optable.xml@.
allowedFromNames :: [String] -> Allowed
allowedFromNames = foldr (\n acc -> acc .|. nameToFlag n) noPrefixes
  where
    nameToFlag "aso"     = pfxAso
    nameToFlag "notrack" = pfxNotrack
    nameToFlag "oso"     = pfxOso
    nameToFlag "rep"     = pfxRep
    nameToFlag "repnz"   = pfxRepnz
    nameToFlag "repz"    = pfxRepz
    nameToFlag "rexb"    = pfxRexb
    nameToFlag "rexr"    = pfxRexr
    nameToFlag "rexx"    = pfxRexx
    nameToFlag "rexw"    = pfxRexw
    nameToFlag "lock"    = pfxLock
    nameToFlag "seg"     = pfxSeg
    nameToFlag other     = error $
      "allowedFromNames: unrecognised prefix name in optable.xml: " ++ show other
