
{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2014-2016
Maintainer  : jhendrix@galois.com

Defines size types in the udis86 file.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
module Flexdis86.Sizes (
    SizeConstraint(..)
  , FPSizeConstraint(..)
  , ModConstraint(..)
  , Fin8
  , asFin8
  , maskFin8
  , unFin8
  , Fin64
  , asFin64
  , unFin64
  ) where

import qualified Control.DeepSeq as DS
import Data.Bits
import Data.Word ( Word8 )
import GHC.Generics

-- | Describes the size of a value.
data SizeConstraint = Size16 | Size32 | Size64 | Size128 | Size256
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData SizeConstraint

-- | Describes whether a floating point memory address value is 32, 64, or 80-bits.
data FPSizeConstraint = FPSize32 | FPSize64 | FPSize80
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData FPSizeConstraint

-- | Identifies whether the mod value of a RM field
-- can be only memory (e.g. !11), only a register (e.g., =11).
data ModConstraint = OnlyMem
                   | OnlyReg
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData ModConstraint

-- | A value 0-7.
newtype Fin8 = Fin8 { unFin8 :: Word8 }
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData Fin8

-- | Project a word to a 'Fin8' if it is between '0' and '7'.
asFin8 :: Word8 -> Maybe Fin8
asFin8 b | 0 <= b && b < 8 = Just (Fin8 b)
         | otherwise = Nothing

-- | Create fin8 from low 3-order bits.
maskFin8 :: Word8 -> Fin8
maskFin8 v = Fin8 (v .&. 0x7)

-- | A value 0-63.
newtype Fin64 = Fin64 { unFin64 :: Word8 }
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData Fin64

-- | Project a word to a 'Fin64' if it is between '0' and '63'.
asFin64 :: Word8 -> Maybe Fin64
asFin64 b | 0 <= b && b < 64 = Just (Fin64 b)
         | otherwise = Nothing
