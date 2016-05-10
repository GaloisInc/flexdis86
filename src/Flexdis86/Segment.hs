{-# LANGUAGE PatternSynonyms #-}
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

import Data.Word ( Word8 )
import Flexdis86.Register

------------------------------------------------------------------------
-- Segment

-- | Refers to a memory segment.
newtype Segment = Segment { _unSegment :: Word8 }
  deriving (Eq, Ord)

pattern ES = Segment 0
pattern CS = Segment 1
pattern SS = Segment 2
pattern DS = Segment 3
pattern FS = Segment 4
pattern GS = Segment 5

instance Show Segment where
  show ES = "es"
  show CS = "cs"
  show SS = "ss"
  show DS = "ds"
  show FS = "fs"
  show GS = "gs"
  show _ = error "internal: illegal segment value"

-- | Return segment register by index or fail.
segmentRegisterByIndex :: Monad m => Word8 -> m Segment
segmentRegisterByIndex r
  | r < 6 = return (Segment r)
  | otherwise = fail "Invalid segment register."

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
