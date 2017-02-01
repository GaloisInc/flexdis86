{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2014-2016
Maintainer  : jhendrix@galois.com

Defines size types in the udis86 file.
-}
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

import Data.Bits
import Data.Word ( Word8 )

-- | Describes whether a value is 16, 32 or 64-bits.
data SizeConstraint = Size16 | Size32 | Size64 | Size128
  deriving (Eq, Ord, Show)

-- | Describes whether a floating point memory address value is 32, 64, or 80-bits.
data FPSizeConstraint = FPSize32 | FPSize64 | FPSize80
  deriving (Eq, Ord, Show)

-- | Identifies whether the mod value of a RM field
-- can be only memory (e.g. !11), only a register (e.g., =11).
data ModConstraint = OnlyMem
                   | OnlyReg
  deriving (Eq, Show, Ord)

-- | A value 0-7.
newtype Fin8 = Fin8 { unFin8 :: Word8 }
  deriving (Eq, Show, Ord)

-- | Project a word to a 'Fin8' if it is between '0' and '7'.
asFin8 :: Word8 -> Maybe Fin8
asFin8 b | 0 <= b && b < 8 = Just (Fin8 b)
         | otherwise = Nothing

-- | Create fin8 from low 3-order bits.
maskFin8 :: Word8 -> Fin8
maskFin8 v = Fin8 (v .&. 0x7)

-- | A value 0-63.
newtype Fin64 = Fin64 { unFin64 :: Word8 }
  deriving (Eq, Show, Ord)

-- | Project a word to a 'Fin64' if it is between '0' and '63'.
asFin64 :: Word8 -> Maybe Fin64
asFin64 b | 0 <= b && b < 64 = Just (Fin64 b)
         | otherwise = Nothing
