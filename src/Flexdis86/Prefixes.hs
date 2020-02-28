{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2014-2016
Maintainer  : jhendrix@galois.com

Defines prefix operations.
-}
{-# LANGUAGE RankNTypes #-}
module Flexdis86.Prefixes
  ( Prefixes(..)
  , REX(..)
  , rexW
  , rexR
  , rexB
  , rexX
  , SegmentPrefix(..)
  , prLockPrefix
  , prSP
  , prREX
  , prVEX
  , prASO
  , prOSO
  , prAddrSize
  , no_seg_prefix
  , setDefault
  , LockPrefix(..)
  , ppLockPrefix
  , VEX(..)
  , vexRex
  , vexVVVV
  , vex256
  ) where

import           Control.Lens
import qualified Data.Bits as B
import           Data.Word ( Word8 )
import           Numeric ( showHex )
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.PrettyPrint.ANSI.Leijen hiding (empty, (<$>))
import           Text.Printf

import           Flexdis86.Segment
import           Flexdis86.Sizes

-- | Prefixes for an instruction.
data Prefixes = Prefixes { _prLockPrefix :: LockPrefix
                         , _prSP  :: SegmentPrefix
                         , _prREX :: REX
                         , _prVEX :: !(Maybe VEX)
                         , _prASO :: Bool
                         , _prOSO :: Bool
                         }
                deriving (Eq, Show)

data VEX = VEX2 Word8{-1-}              -- ^ Byte of a 2-byte VEX prefix
         | VEX3 Word8{-1-} Word8{-2-}   -- ^ Byte 1 and 2 of 3-byte VEX prefix
           deriving (Eq, Show)


vexLens :: (Word8 -> a) ->
           (Word8 -> Word8 -> a) ->
           (Word8 -> a -> Word8) ->
           (Word8 -> Word8 -> a -> (Word8,Word8)) ->
           Lens' VEX a
vexLens get1 get2 upd1 upd2 = lens getter updater
  where getter vex = case vex of
                       VEX2 b     -> get1 b
                       VEX3 b1 b2 -> get2 b1 b2
        updater old a = case old of
                          VEX2 b     -> VEX2 (upd1 b a)
                          VEX3 b1 b2 -> let (b1',b2') = upd2 b1 b2 a
                                        in VEX3 b1' b2'

-- | Are we using 256-bit vectors
vex256 :: Lens' VEX Bool
vex256 = vexLens gt (\_ b2 -> gt b2)
                 st (\b1 b2 a -> (b1, st b2 a))
  where
  gt b   = B.testBit b 2
  st b v = if v then B.setBit b 2 else B.clearBit b 2


-- | REX info from VEX prefix
vexRex :: Lens' VEX REX
vexRex = vexLens gt1 gt2 st1 st2
  where
  gt1 b      = REX (0x40 B..|. B.shiftR (B.complement b B..&. 0x80) 5)
  gt2 b1 b2  = REX (0x40 B..|. B.shiftR (B.complement b2 B..&. 0x80) 4
                         B..|. B.shiftR (B.complement b1 B..&. 0xE0) 5)

  st1 b v     = if v ^. rexR then B.clearBit b 7 else B.setBit b 7
  st2 b1 b2 v = ( (b1 B..&. 0x1F) B..|. (B.complement (unREX v) `B.shiftL` 5)
                , if v ^. rexW then B.clearBit b2 7 else B.setBit b2 7
                )

-- | The VVVV field.  We return it as is.  Note that when used to encode
-- registers, the byte needs to be complemented.
vexVVVV :: Lens' VEX Word8
vexVVVV = vexLens gt (\_ b2 -> gt b2)
                  st (\b1 b2 v -> (b1, st b2 v))
  where
  gt b   = B.shiftR b 3 B..&. 0xF
  st b v = (b B..&. 0x87) B..|. B.shiftL (v B..&. 0xF) 3



-- | REX value for 64-bit mode.
newtype REX = REX { unREX :: Word8 }
  deriving (Eq)

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

-- | Includes segment prefix and branch override hints.
newtype SegmentPrefix = SegmentPrefix { unwrapSegmentPrefix :: Word8 }
  deriving (Eq, Show)

prLockPrefix :: Lens' Prefixes LockPrefix
prLockPrefix = lens _prLockPrefix (\s v -> s { _prLockPrefix = v })

prSP :: Lens' Prefixes SegmentPrefix
prSP = lens _prSP (\s v -> s { _prSP = v})

prREX :: Lens' Prefixes REX
prREX = lens _prREX (\s v -> s { _prREX = v })

prVEX :: Lens' Prefixes (Maybe VEX)
prVEX = lens _prVEX (\s v -> s { _prVEX = v })

prASO :: Lens' Prefixes Bool
prASO = lens _prASO (\s v -> s { _prASO = v })

prOSO :: Lens' Prefixes Bool
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
setDefault (SegmentPrefix 0x26) _ = ES
setDefault (SegmentPrefix 0x2e) _ = CS
setDefault (SegmentPrefix 0x36) _ = SS
setDefault (SegmentPrefix 0x3e) _ = DS
setDefault (SegmentPrefix 0x64) _ = FS
setDefault (SegmentPrefix 0x65) _ = GS
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
