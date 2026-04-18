{- |
Module      : Flexdis86.PrefixSet
Copyright   : (c) Galois, Inc, 2026
Maintainer  : langston@galois.com

A compact Word16 bitset representing the set of prefixes allowed on an
x86 instruction definition.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Flexdis86.PrefixSet
  ( PrefixSet
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
  , prefixSetFromNames
  ) where

import           Data.Bits
import qualified Data.Binary as Bin
import           Data.Binary (Binary)
import qualified Control.DeepSeq as DS
import           Data.Word (Word16)

-- | A set of allowed x86 instruction prefixes, packed as a 'Word16' bitset.
-- Each bit corresponds to one of the twelve prefix names recognised in
-- @optable.xml@.
newtype PrefixSet = PrefixSet Word16
  deriving (Bits, Eq, Ord, Show)

instance DS.NFData PrefixSet where rnf !_ = ()
-- | For "Flexdis86.OpTable.Parse".
instance Binary PrefixSet where
  put (PrefixSet w) = Bin.put w
  get = PrefixSet <$> Bin.get

-- | An empty 'PrefixSet' (no prefixes allowed).
noPrefixes :: PrefixSet
noPrefixes = PrefixSet 0

pfxAso, pfxNotrack, pfxOso, pfxRep, pfxRepnz, pfxRepz :: PrefixSet
pfxRexb, pfxRexr, pfxRexx, pfxRexw, pfxLock, pfxSeg   :: PrefixSet
pfxAso     = PrefixSet (1 `shiftL`  0)
pfxNotrack = PrefixSet (1 `shiftL`  1)
pfxOso     = PrefixSet (1 `shiftL`  2)
pfxRep     = PrefixSet (1 `shiftL`  3)
pfxRepnz   = PrefixSet (1 `shiftL`  4)
pfxRepz    = PrefixSet (1 `shiftL`  5)
pfxRexb    = PrefixSet (1 `shiftL`  6)
pfxRexr    = PrefixSet (1 `shiftL`  7)
pfxRexx    = PrefixSet (1 `shiftL`  8)
pfxRexw    = PrefixSet (1 `shiftL`  9)
pfxLock    = PrefixSet (1 `shiftL` 10)
pfxSeg     = PrefixSet (1 `shiftL` 11)

-- | Test whether a particular prefix flag is set.
hasPfx :: PrefixSet -- ^ Flag to test (e.g. 'pfxRep')
       -> PrefixSet -- ^ The set to test against
       -> Bool
hasPfx flag ps = ps .&. flag /= zeroBits
{-# INLINE hasPfx #-}

-- | Build a 'PrefixSet' from the prefix name strings used in @optable.xml@.
prefixSetFromNames :: [String] -> PrefixSet
prefixSetFromNames = foldr (\n acc -> acc .|. nameToFlag n) noPrefixes
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
      "prefixSetFromNames: unrecognised prefix name in optable.xml: " ++ show other
