{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Flexdis86.Sizes (
    SizeConstraint(..)
  , FPSizeConstraint(..)
  , ModConstraint(..)
  , Fin8(..)
  , Fin64(..)
  ) where

import Data.Word ( Word8 )

-- | Describes whether a value is 16, 32 or 64-bits.
data SizeConstraint = Size16 | Size32 | Size64 | Size128
  deriving (Eq, Show, Ord)

-- | Describes whether a floating point memory address value is 32, 64, or 80-bits.
data FPSizeConstraint = FPSize32 | FPSize64 | FPSize80
  deriving (Eq, Show, Ord)

-- | Identifies whether the mod value of a RM field
-- can be only memory (e.g. !11), only a register (e.g., =11).
data ModConstraint = OnlyMem
                   | OnlyReg
  deriving (Eq, Show, Ord)

-- | A value 0-7.
newtype Fin8 = Fin8 { unFin8 :: Word8 }
  deriving (Eq, Enum, Integral, Num, Ord, Real, Show)

-- | A value 0-63.
newtype Fin64 = Fin64 { unFin64 :: Word8 }
  deriving (Eq, Show, Ord)
