{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2014-2016
Maintainer  : jhendrix@galois.com

Defines a datatype for segments and supporting operations.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}

module Flexdis86.Segment
  ( Segment
  , pattern ES
  , pattern CS
  , pattern SS
  , pattern DS
  , pattern FS
  , pattern GS
  , segmentRegisterByIndex
  , segmentRegNo
  , isDefaultSeg32
  , isDefaultSeg64
  ) where

import qualified Control.DeepSeq as DS
import Data.Word (Word8)
import GHC.Generics
import qualified Prettyprinter as PP

import Flexdis86.Register

------------------------------------------------------------------------
-- Segment

-- | Refers to a memory segment.
newtype Segment = Segment { _unSegment :: Word8 }
  deriving (Eq, Generic, Ord)

instance DS.NFData Segment

pattern ES :: Segment
pattern ES = Segment 0

pattern CS :: Segment
pattern CS = Segment 1

pattern SS :: Segment
pattern SS = Segment 2

pattern DS :: Segment
pattern DS = Segment 3

pattern FS :: Segment
pattern FS = Segment 4

pattern GS :: Segment
pattern GS = Segment 5

{-# COMPLETE ES, CS, SS, DS, FS, GS #-}

instance Show Segment where
  show ES = "es"
  show CS = "cs"
  show SS = "ss"
  show DS = "ds"
  show FS = "fs"
  show GS = "gs"

instance PP.Pretty Segment where
  pretty = PP.unsafeViaShow

-- | Return segment register by index or fail.
segmentRegisterByIndex :: Monad m => Word8 -> m Segment
segmentRegisterByIndex r
  | r < 6 = return (Segment r)
  | otherwise = error "Invalid segment register."

segmentRegNo :: Segment -> Word8
segmentRegNo (Segment r) = r

-- | Return default segment for register
defaultSeg64 :: Reg64 -> Segment
defaultSeg64 RSP = SS
defaultSeg64 RBP = SS
defaultSeg64 _   = DS

isDefaultSeg32 :: Segment -> Reg32 -> Bool
isDefaultSeg32 seg reg = defaultSeg64 (reg32_reg reg) == seg

isDefaultSeg64 :: Segment -> Reg64 -> Bool
isDefaultSeg64 s r = defaultSeg64 r == s
