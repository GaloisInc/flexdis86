
{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2014-2016
Maintainer  : jhendrix@galois.com

Defines size types in the udis86 file.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
module Flexdis86.Sizes (
    SizeConstraint(..)
  , FPSizeConstraint(..)
  , ModConstraint(..)
  , Fin8
  , asFin8
  , maskFin8
  , unFin8
  , MaybeFin8(NothingFin8, JustFin8)
  , maybeFin8ToMaybe
  , maybeFin8
  , matchesMaybeFin8
  , Fin64
  , asFin64
  , unFin64
  ) where

import qualified Control.DeepSeq as DS
import qualified Data.Binary as Bin
import           Data.Binary (Binary)
import           Data.Bits
import           Data.Word ( Word8 )
import           GHC.Generics

-- | Describes the size of a value.
data SizeConstraint = Size16 | Size32 | Size64 | Size128 | Size256
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData SizeConstraint
-- | For "Flexdis86.OpTable.Parse".
instance Binary SizeConstraint

-- | Describes whether a floating point memory address value is 32, 64, or 80-bits.
data FPSizeConstraint = FPSize32 | FPSize64 | FPSize80
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData FPSizeConstraint
-- | For "Flexdis86.OpTable.Parse".
instance Binary FPSizeConstraint

-- | Identifies whether the mod value of a RM field
-- can be only memory (e.g. !11), only a register (e.g., =11).
data ModConstraint = OnlyMem
                   | OnlyReg
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData ModConstraint
-- | For "Flexdis86.OpTable.Parse".
instance Binary ModConstraint

-- | A value 0-7.
newtype Fin8 = Fin8 { unFin8 :: Word8 }
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData Fin8
-- | For "Flexdis86.OpTable.Parse".
instance Binary Fin8

-- | Project a word to a 'Fin8' if it is between '0' and '7'.
asFin8 :: Word8 -> Maybe Fin8
asFin8 b | 0 <= b && b < 8 = Just (Fin8 b)
         | otherwise = Nothing

-- | Create fin8 from low 3-order bits.
maskFin8 :: Word8 -> Fin8
maskFin8 v = Fin8 (v .&. 0x7)

------------------------------------------------------------------------
-- MaybeFin8

-- | A 'Fin8' that may be absent, stored as a single unboxed 'Word8'.
-- The value @8@ encodes the absent case; values @0@–@7@ encode 'Fin8'.
-- This avoids the two-word overhead of @'Maybe' 'Fin8'@.
--
-- __Invariant:__ the underlying 'Word8' is always in the range @0@–@8@;
-- values @9@–@255@ are never used.
newtype MaybeFin8 = MaybeFin8 Word8
  deriving (Eq, Ord, Show)

instance DS.NFData MaybeFin8 where rnf !_ = ()
-- | For "Flexdis86.OpTable.Parse".
instance Binary MaybeFin8 where
  put (MaybeFin8 w) = Bin.put w
  get = MaybeFin8 <$> Bin.get

-- | The absent case (@Nothing@).
pattern NothingFin8 :: MaybeFin8
pattern NothingFin8 = MaybeFin8 8

-- | A present 'Fin8' (@Just@).
pattern JustFin8 :: Fin8 -> MaybeFin8
pattern JustFin8 f <- (maybeFin8ToMaybe -> Just f)
  where JustFin8 (Fin8 w) = MaybeFin8 w

{-# COMPLETE NothingFin8, JustFin8 :: MaybeFin8 #-}

-- | Convert to @'Maybe' 'Fin8'@.
maybeFin8ToMaybe :: MaybeFin8 -> Maybe Fin8
maybeFin8ToMaybe (MaybeFin8 8) = Nothing
maybeFin8ToMaybe (MaybeFin8 w) = Just (Fin8 w)
{-# INLINE maybeFin8ToMaybe #-}

-- | Like 'maybe' for 'MaybeFin8'.
maybeFin8 :: b -> (Fin8 -> b) -> MaybeFin8 -> b
maybeFin8 def _ NothingFin8   = def
maybeFin8 _   f (JustFin8 x)  = f x
{-# INLINE maybeFin8 #-}

-- | Return true if a 'Fin8' satisfies a 'MaybeFin8' constraint
-- (@NothingFin8@ means any value is accepted).
matchesMaybeFin8 :: Fin8 -> MaybeFin8 -> Bool
matchesMaybeFin8 _ NothingFin8   = True
matchesMaybeFin8 i (JustFin8 c)  = i == c
{-# INLINE matchesMaybeFin8 #-}

-- | A value 0-63.
newtype Fin64 = Fin64 { unFin64 :: Word8 }
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData Fin64
-- | For "Flexdis86.OpTable.Parse".
instance Binary Fin64

-- | Project a word to a 'Fin64' if it is between '0' and '63'.
asFin64 :: Word8 -> Maybe Fin64
asFin64 b | 0 <= b && b < 64 = Just (Fin64 b)
         | otherwise = Nothing
