{- |
Module      : Flexdis86.Prefixes.REX
Copyright   : (c) Galois, Inc, 2026

The @REX@ prefix byte and its bit lenses.
-}

{-# LANGUAGE DeriveGeneric #-}

module Flexdis86.Prefixes.REX
  ( REX(..)
  , rexW
  , rexR
  , rexX
  , rexB
  ) where

import qualified Control.DeepSeq as DS
import qualified Data.Bits as B
import           Data.Word (Word8)
import           GHC.Generics
import           Lens.Micro (Lens', lens)
import           Text.Printf

-- | REX value for 64-bit mode.
newtype REX = REX { unREX :: Word8 }
  deriving (Eq, Generic)

instance DS.NFData REX

setBitTo :: (B.Bits b) => b -> Int -> Bool -> b
setBitTo bits bitNo val
  | val = B.setBit bits bitNo
  | otherwise = B.clearBit bits bitNo

-- | Indicates if 64-bit operand size should be used.
rexW :: Lens' REX Bool
rexW = lens ((`B.testBit` 3) . unREX) (\(REX r) v -> REX (setBitTo r 3 v))

-- | Extension of ModR/M reg field.
rexR :: Lens' REX Bool
rexR = lens ((`B.testBit` 2) . unREX) (\(REX r) v -> REX (setBitTo r 2 v))

-- | Extension of SIB index field.
rexX :: Lens' REX Bool
rexX = lens ((`B.testBit` 1) . unREX) (\(REX r) v -> REX (setBitTo r 1 v))

-- | Extension of ModR/M r/m field, SIB base field, or Opcode reg field.
rexB :: Lens' REX Bool
rexB = lens ((`B.testBit` 0) . unREX) (\(REX r) v -> REX (setBitTo r 0 v))

instance Show REX where
  show (REX rex) = printf "0b%08b" rex
