{-| Shared 'Word16' layout for the prefix-tracking modules
"Flexdis86.Prefixes.Seen" and "Flexdis86.Prefixes.Allowed". This module
defines bit positions and a 'containedIn' primitive; the meaning of each
bit is a property of the consuming module.

Validation - checking whether one 'PrefixCode' is a subset of another -
is a single bitwise compare over the shared bits:

@
  (a .&. complement b) .&. 'sharedBitsMask' == 0
@

Upper bits outside 'sharedBitsMask' are reserved for consumer-specific
flags that must not participate in validation.

Bit layout:

@
  [0]   REX.B
  [1]   REX.X
  [2]   REX.R
  [3]   REX.W
  [4]   LOCK
  [5]   REPNZ
  [6]   any REX
  [7]   REP
  [8]   OSO
  [9]   ASO
  [10]  seg code bit 0
  [11]  seg code bit 1
  [12]  seg code bit 2 (also the DS/notrack bit)
  [13]  (free - see "Flexdis86.Prefixes.Allowed")
  [14]  (free - see "Flexdis86.Prefixes.Allowed")
  [15]  (unused)
@

The low byte of the 'PrefixCode' is the raw REX prefix byte (or @0@ if no REX
byte was observed). Because a valid REX byte has the fixed pattern @0b0100WRXB@,
writing it verbatim places @W@\/@R@\/@X@\/@B@ at bits 3\/2\/1\/0 and the
fixed @1@ at bit 6 ('rexSeenBit'). The always-zero REX-byte bits 4, 5, 7 host
additional shared fields without colliding with REX observation.

Some fields are packed with extra bits to make the subset property hold:

* /Lock\/rep/ uses three separate 1-bit positions rather than a 2-bit
  enum. Otherwise a set that allowed both LOCK and REP would spuriously
  admit an observation of REPNZ, since REPNZ's enum code would be a
  subset of the union.

* /Segment/ uses a 3-bit field whose six assigned codes union to
  @0b111@, with DS at the single-bit position 'segDsBit'. This lets a
  mask of @0b111@ admit any segment observation while a mask of
  @0b100@ admits only DS (used for @notrack@-only allowances).
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Flexdis86.Prefixes.Code
  ( PrefixCode(..)
  , emptyPrefixCode
  , containedIn
    -- * Bit layout
  , sharedBitsMask
    -- ** Shared bits
  , rexBBit
  , rexXBit
  , rexRBit
  , rexWBit
  , rexSeenBit
  , rexMask
  , lockBit
  , repNZBit
  , repBit
  , osoBit
  , asoBit
  , segBit0
  , segBit1
  , segDsBit
  , segMask
  ) where

import           Control.Exception (assert)
import qualified Control.DeepSeq as DS
import           Data.Binary (Binary)
import           Data.Bits (Bits, (.&.), (.|.), complement, shiftL, zeroBits)
import           Data.Word (Word16)

-- | Shared bitwise prefix representation. See the module haddock for the
-- layout.
newtype PrefixCode = PrefixCode Word16
  deriving (Binary, Bits, Eq, Ord, Show, DS.NFData)

-- | A 'PrefixCode' with no bits set.
emptyPrefixCode :: PrefixCode
emptyPrefixCode = PrefixCode 0

-- | Mask covering the bits that participate in validation: REX sub-bits,
-- 'rexSeenBit', the legacy-prefix group, segment field, and OSO/ASO.
-- Consumer-defined flags at bits 13, 14 are excluded.
sharedBitsMask :: PrefixCode
sharedBitsMask = PrefixCode 0b0001111111111111

-- | Is every shared bit set in the first 'PrefixCode' also set in the
-- second? Used for validating a 'Seen' against an 'Allowed'. The first
-- argument must not have any bits outside 'sharedBitsMask' set (i.e.
-- must be a 'Seen'-side value); the second may (e.g. it may be an
-- 'Allowed' with extra semantic flags, which are masked off here).
containedIn :: PrefixCode -> PrefixCode -> Bool
containedIn a_ b =
  let a = assert (a_ .&. complement sharedBitsMask == zeroBits) a_
  in a .&. complement (b .&. sharedBitsMask) == zeroBits
{-# INLINE containedIn #-}

-- REX sub-bits occupy the low nibble so that writing the raw REX byte
-- verbatim into the low byte of the 'PrefixCode' aligns W\/R\/X\/B with
-- 'rexWBit'\/'rexRBit'\/'rexXBit'\/'rexBBit'.
rexBBit, rexXBit, rexRBit, rexWBit :: PrefixCode
rexBBit = PrefixCode (1 `shiftL` 0)
rexXBit = PrefixCode (1 `shiftL` 1)
rexRBit = PrefixCode (1 `shiftL` 2)
rexWBit = PrefixCode (1 `shiftL` 3)

-- | \"Any REX prefix byte was observed.\" Because a valid REX byte has
-- the fixed pattern @0b0100WRXB@, writing the REX byte verbatim into the
-- low byte of the 'PrefixCode' always sets this bit. A bare @0x40@ byte
-- (no REX sub-bits set) is semantically distinguishable from no REX at
-- all - it forces 8-bit register operand encodings to use @SIL@\/@DIL@
-- etc. instead of the high-byte @CH@\/@DH@\/etc. registers - so this
-- bit matters.
rexSeenBit :: PrefixCode
rexSeenBit = PrefixCode (1 `shiftL` 6)

-- | Mask covering all four REX sub-bits plus 'rexSeenBit'.
rexMask :: PrefixCode
rexMask = rexBBit .|. rexXBit .|. rexRBit .|. rexWBit .|. rexSeenBit

-- Lock\/rep group is split into three 1-bit positions. Positions 4, 5, 7
-- are REX-byte bits that are always zero in a valid REX byte, so they
-- don't collide with REX observation.
lockBit, repNZBit, repBit :: PrefixCode
lockBit  = PrefixCode (1 `shiftL` 4)
repNZBit = PrefixCode (1 `shiftL` 5)
repBit   = PrefixCode (1 `shiftL` 7)

osoBit, asoBit :: PrefixCode
osoBit = PrefixCode (1 `shiftL` 8)
asoBit = PrefixCode (1 `shiftL` 9)

-- | Segment-code bits. The 3-bit field at positions [10,11,12] encodes
-- the specific segment override observed. Codes are assigned so that
-- every observation is a subset of 'segMask' (for allowances permitting
-- any segment) or of just 'segDsBit' (for allowances that admit only
-- the DS\/notrack byte).
segBit0, segBit1, segDsBit :: PrefixCode
segBit0  = PrefixCode (1 `shiftL` 10)
segBit1  = PrefixCode (1 `shiftL` 11)
segDsBit = PrefixCode (1 `shiftL` 12)

-- | Mask covering all three segment-code bits.
segMask :: PrefixCode
segMask = segBit0 .|. segBit1 .|. segDsBit

