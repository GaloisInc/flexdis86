{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2014-2016
Maintainer  : jhendrix@galois.com

Defines prefix operations.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Flexdis86.Prefixes
  ( Prefixes(..)
  , SegmentPrefix(..)
  , prLockPrefix
  , prSP
  , prREX
  , prVEX
  , prASO
  , prOSO
  , prNoTrack
  , prAddrSize
  , no_seg_prefix
  , notrackPrefixByte
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
  , setDefault
  , LockPrefix(..)
  , ppLockPrefix
  ) where

import qualified Control.DeepSeq as DS
import qualified Data.Bits as B
import           Data.Word ( Word8 )
import           GHC.Generics
import           Lens.Micro (Lens', lens, (^.))
import           Numeric ( showHex )
import qualified Prettyprinter as PP

import           Flexdis86.Prefixes.REX (REX)
import qualified Flexdis86.VEX.Seen as VEX
import           Flexdis86.Segment
import           Flexdis86.Sizes

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

------------------------------------------------------------------------
-- SegmentPrefix

-- | Includes segment prefix and branch override hints.
newtype SegmentPrefix = SegmentPrefix { unwrapSegmentPrefix :: Word8 }
  deriving (Eq, Generic, Show)

instance DS.NFData SegmentPrefix

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
  deriving (Eq, Generic, Show)

instance DS.NFData LockPrefix

ppLockPrefix :: LockPrefix -> PP.Doc a
ppLockPrefix NoLockPrefix = ""
ppLockPrefix LockPrefix = "lock"
ppLockPrefix RepPrefix  = "rep"
ppLockPrefix RepZPrefix = "repz"
ppLockPrefix RepNZPrefix = "repnz"

-----------------------------------------------------------------------
-- REX

-----------------------------------------------------------------------
-- Prefixes

-- | Prefixes for an instruction.
data Prefixes = Prefixes { _prLockPrefix :: !LockPrefix
                         , _prSP  :: !SegmentPrefix
                         , _prREX :: !REX
                         , _prVEX :: !VEX.VEX
                         , _prASO :: !Bool
                         , _prOSO :: !Bool
                         , _prNoTrack :: !Bool
                         }
                deriving (Eq, Generic, Show)

instance DS.NFData Prefixes

prLockPrefix :: Lens' Prefixes LockPrefix
prLockPrefix = lens _prLockPrefix (\s v -> s { _prLockPrefix = v })

prSP :: Lens' Prefixes SegmentPrefix
prSP = lens _prSP (\s v -> s { _prSP = v})

prREX :: Lens' Prefixes REX
prREX = lens _prREX (\s v -> s { _prREX = v })

prVEX :: Lens' Prefixes VEX.VEX
prVEX = lens _prVEX (\s v -> s { _prVEX = v })

prASO :: Lens' Prefixes Bool
prASO = lens _prASO (\s v -> s { _prASO = v })

prOSO :: Lens' Prefixes Bool
prOSO = lens _prOSO (\s v -> s { _prOSO = v })

prNoTrack :: Lens' Prefixes Bool
prNoTrack = lens _prNoTrack (\s v -> s { _prNoTrack = v })

prAddrSize :: Prefixes -> SizeConstraint
prAddrSize pfx | pfx^.prASO = Size32
               | otherwise  = Size64
