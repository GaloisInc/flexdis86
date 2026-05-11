{- |
Module      : Flexdis86.Prefixes.Required
Copyright   : (c) Galois, Inc, 2026

The required-prefix mask for a 'Def', represented as a
'Flexdis86.Prefixes.Code.PrefixCode'. Most instructions have no required
prefix ('noRequired'); SSE instructions and a handful of special cases
carry exactly one required byte (0x66, 0xF2, or 0xF3).

A 'Required' is used in two ways during validation:

  1. Its bit(s) are checked against the 'Flexdis86.Prefixes.Seen.Seen'
     accumulator to confirm the required byte was actually observed.

  2. Its bit(s) are cleared from the 'Flexdis86.Prefixes.Seen.Seen'
     accumulator before materializing legacy-prefix semantics.

The OR of a 'Required' into the allowed mask is precomputed at parse time
and stored in 'Flexdis86.OpTable.Def._defPrefix' (the effective-allowed
mask), so validation needs no runtime OR.

Build a 'Required' with 'requiredFromByte'; use 'noRequired' when a 'Def'
carries no required prefix.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Flexdis86.Prefixes.Required
  ( Required
  , requiredCode
  , noRequired
  , requiredFromByte
  , requiredToByte
  , orRequired
  ) where

import qualified Control.DeepSeq as DS
import           Data.Binary (Binary)
import           Data.Bits (Bits, zeroBits, (.|.))
import           Data.Word (Word8)

import           Flexdis86.Prefixes.Allowed (Allowed(..))

import           Flexdis86.Prefixes
import           Flexdis86.Prefixes.Code

-- | The required-prefix bit mask for a 'Def'. Wraps a 'PrefixCode'; only
-- the shared bits are ever set (bits 13 and 14 are always zero).
newtype Required = Required { requiredCode :: PrefixCode }
  deriving (Bits, Binary, DS.NFData, Eq, Ord, Show)

-- | No required prefix: the 'Def' does not mandate any particular prefix
-- byte.
noRequired :: Required
noRequired = Required zeroBits

-- | Build a 'Required' from a raw prefix byte. The only bytes the optable
-- uses for required prefixes are 0x66, 0xF2, and 0xF3; any other value
-- yields 'noRequired'.
requiredFromByte :: Word8 -> Required
requiredFromByte b
  | b == operandSizeOverrideByte = Required osoBit
  | b == repNZPrefixByte         = Required repNZBit
  | b == repPrefixByte           = Required repBit
  | otherwise                    = noRequired

-- | Recover the raw prefix byte from a 'Required'. Inverse of
-- 'requiredFromByte'; returns 'Nothing' for 'noRequired'.
requiredToByte :: Required -> Maybe Word8
requiredToByte (Required rc)
  | rc == osoBit   = Just operandSizeOverrideByte
  | rc == repNZBit = Just repNZPrefixByte
  | rc == repBit   = Just repPrefixByte
  | otherwise      = Nothing

-- | OR the required-prefix bit(s) into an 'Allowed' mask, producing the
-- effective-allowed value used by
-- 'Flexdis86.Prefixes.Code.containedIn'.
orRequired :: Allowed -> Required -> Allowed
orRequired (Allowed ac) (Required rc) = Allowed (ac .|. rc)
