{- |
Module      : Flexdis86.Prefixes.Bytes
Copyright   : (c) Galois, Inc, 2014-2026

Raw prefix byte constants. Broken out from "Flexdis86.Prefixes" so that
lower-level modules ("Flexdis86.Prefixes.Seen",
"Flexdis86.Prefixes.Required") can refer to them without importing
"Flexdis86.Prefixes" itself.
-}

module Flexdis86.Prefixes.Bytes
  ( notrackPrefixByte
  , addrSizeOverrideByte
  , operandSizeOverrideByte
  , lockPrefixByte
  , repNZPrefixByte
  , repPrefixByte
  , esPrefixByte
  , csPrefixByte
  , ssPrefixByte
  , dsPrefixByte
  , fsPrefixByte
  , gsPrefixByte
  , isRexPrefixByte
  ) where

import qualified Data.Bits as B
import           Data.Word (Word8)

-- | The byte used for the @notrack@ CET prefix.
--
-- This should not be confused with the DS segment override, which uses the same
-- byte. Instructions uphold the convention that notrack and segment overrides
-- are not used simultaneously to avoid ambiguity.
notrackPrefixByte :: Word8
notrackPrefixByte = 0x3e

-- | The address-size override prefix byte.
addrSizeOverrideByte :: Word8
addrSizeOverrideByte = 0x67

-- | The operand-size override prefix byte.
operandSizeOverrideByte :: Word8
operandSizeOverrideByte = 0x66

-- | The @LOCK@ prefix byte.
lockPrefixByte :: Word8
lockPrefixByte = 0xF0

-- | The @REPNZ@/@REPNE@ prefix byte.
repNZPrefixByte :: Word8
repNZPrefixByte = 0xF2

-- | The @REP@/@REPZ@/@REPE@ prefix byte.
repPrefixByte :: Word8
repPrefixByte = 0xF3

-- | The segment-override prefix bytes: ES, CS, SS, DS, FS, GS.
--
-- Note that the DS byte (0x3E) is the same byte as 'notrackPrefixByte'; which
-- role it plays depends on the instruction.
esPrefixByte, csPrefixByte, ssPrefixByte, dsPrefixByte :: Word8
fsPrefixByte, gsPrefixByte :: Word8
esPrefixByte = 0x26
csPrefixByte = 0x2E
ssPrefixByte = 0x36
dsPrefixByte = 0x3E
fsPrefixByte = 0x64
gsPrefixByte = 0x65

-- | Is the given byte a REX prefix (0x40..0x4F)?
isRexPrefixByte :: Word8 -> Bool
isRexPrefixByte b = b B..&. 0xF0 == 0x40
