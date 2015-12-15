module Flexdis86.Segment (
    Segment, es, cs, ss, ds, fs, gs, segmentRegisterByIndex, segmentRegNo
  , isDefaultSeg32, isDefaultSeg64
  ) where

import Data.Word ( Word8 )
import Flexdis86.Register

------------------------------------------------------------------------
-- Segment

-- | Refers to a memory segment.
newtype Segment = Segment { _unSegment :: Word8 }
  deriving (Eq, Ord)

instance Show Segment where
  show (Segment 0x0) = "es"
  show (Segment 0x1) = "cs"
  show (Segment 0x2) = "ss"
  show (Segment 0x3) = "ds"
  show (Segment 0x4) = "fs"
  show (Segment 0x5) = "gs"
  show _ = error "internal: illegal segment value"

-- | Return segment register by index or fail.
segmentRegisterByIndex :: Monad m => Word8 -> m Segment
segmentRegisterByIndex r
  | r < 6 = return (Segment r)
  | otherwise = fail "Invalid segment register."

segmentRegNo :: Segment -> Word8
segmentRegNo (Segment r) = r

es :: Segment
es = Segment 0

cs :: Segment
cs = Segment 1

ss :: Segment
ss = Segment 2

ds :: Segment
ds = Segment 3

fs :: Segment
fs = Segment 4

gs :: Segment
gs = Segment 5

isDefaultSeg32 :: Segment -> Reg32 -> Bool
isDefaultSeg32 seg reg = isDefaultSeg64 seg $ reg32_reg reg

isDefaultSeg64 :: Segment -> Reg64 -> Bool
isDefaultSeg64 (Segment 2) (Reg64  4) = True
isDefaultSeg64 (Segment 2) (Reg64  5) = True
isDefaultSeg64 (Segment 3) (Reg64  0) = True
isDefaultSeg64 (Segment 3) (Reg64  1) = True
isDefaultSeg64 (Segment 3) (Reg64  2) = True
isDefaultSeg64 (Segment 3) (Reg64  3) = True
isDefaultSeg64 (Segment 3) (Reg64  6) = True
isDefaultSeg64 (Segment 3) (Reg64  7) = True
isDefaultSeg64 (Segment 3) (Reg64  8) = True
isDefaultSeg64 (Segment 3) (Reg64  9) = True
isDefaultSeg64 (Segment 3) (Reg64 10) = True
isDefaultSeg64 (Segment 3) (Reg64 11) = True
isDefaultSeg64 (Segment 3) (Reg64 12) = True
isDefaultSeg64 (Segment 3) (Reg64 13) = True
isDefaultSeg64 (Segment 3) (Reg64 14) = True
isDefaultSeg64 (Segment 3) (Reg64 15) = True
isDefaultSeg64 _ _ = False
