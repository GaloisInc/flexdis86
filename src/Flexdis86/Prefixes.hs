module Flexdis86.Prefixes (
  Prefixes(..),
  REX(..),
  rexW,
  rexR,
  rexB,
  rexX,
  SegmentPrefix(..),
  prLockPrefix,
  prSP,
  prREX,
  prASO,
  prOSO,
  prAddrSize,
  no_seg_prefix,
  setDefault,
  LockPrefix(..),
  ppLockPrefix
  ) where

import Control.Lens
import qualified Data.Bits as B
import Data.Word ( Word8 )
import Numeric ( showHex )
import Text.PrettyPrint.ANSI.Leijen hiding (empty, (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Flexdis86.Segment
import Flexdis86.Sizes

-- | Prefixes for an instruction.
data Prefixes = Prefixes { _prLockPrefix :: LockPrefix
                         , _prSP  :: SegmentPrefix
                         , _prREX :: REX
                         , _prASO :: Bool
                         , _prOSO :: Bool
                         }
                deriving (Eq, Show)

-- | REX value for 64-bit mode.
newtype REX = REX { unREX :: Word8 }
  deriving (Eq)

setBitTo :: (B.Bits b) => b -> Int -> Bool -> b
setBitTo bits bitNo val
  | val = B.setBit bits bitNo
  | otherwise = B.clearBit bits bitNo

rexW :: Simple Lens REX Bool
rexW = lens ((`B.testBit` 3) . unREX) (\(REX r) v -> REX (setBitTo r 3 v))

rexR :: Simple Lens REX Bool
rexR = lens ((`B.testBit` 2) . unREX) (\(REX r) v -> REX (setBitTo r 2 v))

rexX :: Simple Lens REX Bool
rexX = lens ((`B.testBit` 1) . unREX) (\(REX r) v -> REX (setBitTo r 1 v))

rexB :: Simple Lens REX Bool
rexB = lens ((`B.testBit` 0) . unREX) (\(REX r) v -> REX (setBitTo r 0 v))

instance Show REX where
  showsPrec _ (REX rex) = showHex rex

-- | Includes segment prefix and branch override hints.
newtype SegmentPrefix = SegmentPrefix Word8
  deriving (Eq, Show)

prLockPrefix :: Simple Lens Prefixes LockPrefix
prLockPrefix = lens _prLockPrefix (\s v -> s { _prLockPrefix = v })

prSP :: Simple Lens Prefixes SegmentPrefix
prSP = lens _prSP (\s v -> s { _prSP = v})

prREX :: Simple Lens Prefixes REX
prREX = lens _prREX (\s v -> s { _prREX = v })

prASO :: Simple Lens Prefixes Bool
prASO = lens _prASO (\s v -> s { _prASO = v })

prOSO :: Simple Lens Prefixes Bool
prOSO = lens _prOSO (\s v -> s { _prOSO = v })

prAddrSize :: Prefixes -> SizeConstraint
prAddrSize pfx | pfx^.prASO = Size32
               | otherwise  = Size64


------------------------------------------------------------------------
-- SegmentPrefix

no_seg_prefix :: SegmentPrefix
no_seg_prefix = SegmentPrefix 0

setDefault :: SegmentPrefix -> Segment -> Segment
setDefault (SegmentPrefix 0) s = s
setDefault (SegmentPrefix 0x26) _ = es
setDefault (SegmentPrefix 0x2e) _ = cs
setDefault (SegmentPrefix 0x36) _ = ss
setDefault (SegmentPrefix 0x3e) _ = ds
setDefault (SegmentPrefix 0x64) _ = fs
setDefault (SegmentPrefix 0x65) _ = gs
setDefault (SegmentPrefix w) _ = error $ "Unexpected segment prefix: " ++ showHex w ""

data LockPrefix
   = NoLockPrefix
   | LockPrefix
   | RepPrefix
   | RepZPrefix
   | RepNZPrefix
  deriving (Show, Eq)

ppLockPrefix :: LockPrefix -> Doc
ppLockPrefix NoLockPrefix = PP.empty
ppLockPrefix LockPrefix = text "lock"
ppLockPrefix RepPrefix  = text "rep"
ppLockPrefix RepZPrefix = text "repz"
ppLockPrefix RepNZPrefix = text "repnz"
