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
  , module Flexdis86.Prefixes.Bytes
  , setDefault
  , LockPrefix(..)
  , ppLockPrefix
  ) where

import qualified Control.DeepSeq as DS
import           Data.Word ( Word8 )
import           GHC.Generics
import           Lens.Micro (Lens', lens, (^.))
import           Numeric ( showHex )
import qualified Prettyprinter as PP

import           Flexdis86.Prefixes.Bytes
import           Flexdis86.Prefixes.REX (REX)
import qualified Flexdis86.VEX.Seen as VEX
import           Flexdis86.Segment
import           Flexdis86.Sizes

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
