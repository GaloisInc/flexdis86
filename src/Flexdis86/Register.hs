{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2014-2016
Maintainer  : jhendrix@galois.com

Defines types for x86 registers.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
module Flexdis86.Register (
    -- * 8-bit General Purpose registers
    Reg8
  , pattern LowReg8
  , pattern HighReg8
  , pattern AL
  , pattern BL
  , pattern CL
  , pattern DL
  , pattern SPL
  , pattern BPL
  , pattern SIL
  , pattern DIL
  , pattern AH
  , pattern BH
  , pattern CH
  , pattern DH
    -- * 16-bit General Purpose registers
  , Reg16(..)
  , reg16, reg16_reg
  , pattern AX
  , pattern BX
  , pattern CX
  , pattern DX
  , pattern SP
  , pattern BP
  , pattern SI
  , pattern DI
    -- * 32-bit General Purpose registers
  , Reg32(..)
  , reg32_reg
  , pattern EAX
  , pattern EBX
  , pattern ECX
  , pattern EDX
  , pattern ESP
  , pattern EBP
  , pattern ESI
  , pattern EDI
    -- * 64-bit General Purpose registers
  , Reg64(..), reg64, reg64No, reg64Idx
  , pattern RAX
  , pattern RBX
  , pattern RCX
  , pattern RDX
  , pattern RSP
  , pattern RBP
  , pattern RSI
  , pattern RDI
  , pattern R8
  , pattern R9
  , pattern R10
  , pattern R11
  , pattern R12
  , pattern R13
  , pattern R14
  , pattern R15
    -- * Control registers
  , ControlReg(..), controlReg, controlRegNo
    -- * Debug registers
  , DebugReg(..), debugReg, debugRegNo
    -- * MMX registers
  , MMXReg(..), mmxReg, mmxRegNo, mmxRegIdx
    -- * XMM registers
  , XMMReg(..), xmmReg, xmmRegNo, xmmRegIdx
    -- * YMM registers
  , YMMReg(..), ymmReg, ymmRegNo, ymmRegIdx
  ) where

import           Control.Exception ( assert )
import           Data.Bits
import qualified Data.Vector as V
import           Data.Word ( Word8 )

------------------------------------------------------------------------
-- Reg8

-- | We use 0 to 15 to correspond to denote the low-order bytes
-- of the registers, and 16-19 to denote bits 8-15 of regists
-- rax, rcx, rdx, and rbx respectively.
newtype Reg8 = Reg8 Word8
  deriving (Eq, Ord)

asLowReg :: Reg8 -> Maybe Word8
asLowReg (Reg8 w) | w < 16 = Just w
                  | otherwise = Nothing

low_reg8 :: Word8 -> Reg8
low_reg8 w | w < 16 = Reg8 w
           | otherwise = error $ "low_reg8 given bad index " ++ show w

pattern LowReg8 :: Word8 -> Reg8
pattern LowReg8 w <- (asLowReg -> Just w)
  where LowReg8 x = low_reg8 x

asHighReg :: Reg8 -> Maybe Word8
asHighReg (Reg8 w) | w >= 16 = Just (w .&. 0xf)
                   | otherwise = Nothing

-- | Returns the high register ah, ch, dh, or bh from the index value.
high_reg8 :: Word8 -> Reg8
high_reg8 w | w < 4 = Reg8 $ 16+w
            | otherwise = error $ "high_reg8 given bad index " ++ show w

-- | One of the 4 registers ah ch dh bh
pattern HighReg8 :: Word8 -> Reg8
pattern HighReg8 w <- (asHighReg -> Just w)
  where HighReg8 x = high_reg8 x

instance Show Reg8 where
  show (Reg8 i) = assert (i < 20) (regNames8 V.! (fromIntegral i))

regNames8 :: V.Vector String
regNames8 = V.fromList [ "al",   "cl",   "dl",   "bl"
                       , "spl",  "bpl",  "sil",  "dil"
                       , "r8b" , "r9b" , "r10b", "r11b"
                       , "r12b", "r13b", "r14b", "r15b"
                       , "ah",   "ch",   "dh",   "bh"
                       ]

pattern AL :: Reg8
pattern AL =  Reg8 0

pattern CL :: Reg8
pattern CL =  Reg8 1

pattern DL :: Reg8
pattern DL =  Reg8 2

pattern BL :: Reg8
pattern BL =  Reg8 3

pattern SPL :: Reg8
pattern SPL =  Reg8 4

pattern BPL :: Reg8
pattern BPL =  Reg8 5

pattern SIL :: Reg8
pattern SIL =  Reg8 6

pattern DIL :: Reg8
pattern DIL =  Reg8 7

pattern AH :: Reg8
pattern AH = HighReg8 0

pattern CH :: Reg8
pattern CH = HighReg8 1

pattern DH :: Reg8
pattern DH = HighReg8 2

pattern BH :: Reg8
pattern BH = HighReg8 3

------------------------------------------------------------------------
-- Reg16

-- | We always get the low order 16-bits of a 64-bit register.
newtype Reg16 = Reg16 Word8
  deriving (Eq, Ord)

reg16 :: Word8 -> Reg16
reg16 i = assert (i < 16) (Reg16 i)

reg16_reg :: Reg16 -> Reg64
reg16_reg (Reg16 r) = Reg64 r

instance Show Reg16 where
  show (Reg16 i) = assert (i < 16) (regNames16 V.! fromIntegral i)

regNames16 :: V.Vector String
regNames16 = V.fromList [ "ax",   "cx",   "dx",   "bx"
                        , "sp",   "bp",   "si",   "di"
                        , "r8w" , "r9w" , "r10w", "r11w"
                        , "r12w", "r13w", "r14w", "r15w"
                        ]

pattern AX :: Reg16
pattern AX = Reg16 0

pattern CX :: Reg16
pattern CX = Reg16 1

pattern DX :: Reg16
pattern DX = Reg16 2

pattern BX :: Reg16
pattern BX = Reg16 3

pattern SP :: Reg16
pattern SP = Reg16 4

pattern BP :: Reg16
pattern BP = Reg16 5

pattern SI :: Reg16
pattern SI = Reg16 6

pattern DI :: Reg16
pattern DI = Reg16 7

------------------------------------------------------------------------
-- Reg32

-- | We always get the low order 32-bits of a 64-bit register.
newtype Reg32 = Reg32 Word8
  deriving (Eq, Ord)

reg32_reg :: Reg32 -> Reg64
reg32_reg (Reg32 r) = Reg64 r

instance Show Reg32 where
  show (Reg32 i) = assert (i < 16) (regNames32 V.! fromIntegral i)

regNames32 :: V.Vector String
regNames32 = V.fromList [ "eax",  "ecx",  "edx",  "ebx"
                        , "esp",  "ebp",  "esi",  "edi"
                        , "r8d" , "r9d" , "r10d", "r11d"
                        , "r12d", "r13d", "r14d", "r15d"
                        ]

pattern EAX :: Reg32
pattern EAX = Reg32 0

pattern ECX :: Reg32
pattern ECX = Reg32 1

pattern EDX :: Reg32
pattern EDX = Reg32 2

pattern EBX :: Reg32
pattern EBX = Reg32 3

pattern ESP :: Reg32
pattern ESP = Reg32 4

pattern EBP :: Reg32
pattern EBP = Reg32 5

pattern ESI :: Reg32
pattern ESI = Reg32 6

pattern EDI :: Reg32
pattern EDI = Reg32 7

------------------------------------------------------------------------
-- Reg64

newtype Reg64 = Reg64 { unReg64 :: Word8 }
  deriving (Eq, Ord)

reg64 :: Word8 -> Reg64
reg64 = Reg64

{-# DEPRECATED reg64 "Use Reg64 instead" #-}

reg64No :: Reg64 -> Word8
reg64No (Reg64 r) = r

-- | Return index of 64-bit register.
reg64Idx :: Reg64 -> Int
reg64Idx = fromIntegral . unReg64

instance Show Reg64 where
  show (Reg64 i) = assert (i < 16) (regNames64 V.! fromIntegral i)

regNames64 :: V.Vector String
regNames64 = V.fromList [ "rax", "rcx", "rdx", "rbx"
                        , "rsp", "rbp", "rsi", "rdi"
                        , "r8" , "r9" , "r10", "r11"
                        , "r12", "r13", "r14", "r15"
                        ]


pattern RAX :: Reg64
pattern RAX = Reg64  0

pattern RCX :: Reg64
pattern RCX = Reg64  1

pattern RDX :: Reg64
pattern RDX = Reg64  2

pattern RBX :: Reg64
pattern RBX = Reg64  3

pattern RSP :: Reg64
pattern RSP = Reg64  4

pattern RBP :: Reg64
pattern RBP = Reg64  5

pattern RSI :: Reg64
pattern RSI = Reg64  6

pattern RDI :: Reg64
pattern RDI = Reg64  7

pattern R8  :: Reg64
pattern R8  = Reg64  8

pattern R9  :: Reg64
pattern R9  = Reg64  9

pattern R10 :: Reg64
pattern R10 = Reg64 10

pattern R11 :: Reg64
pattern R11 = Reg64 11

pattern R12 :: Reg64
pattern R12 = Reg64 12

pattern R13 :: Reg64
pattern R13 = Reg64 13

pattern R14 :: Reg64
pattern R14 = Reg64 14

pattern R15 :: Reg64
pattern R15 = Reg64 15

------------------------------------------------------------------------
-- ControlReg

-- | There are 16 control registers CR0 through CR15.
newtype ControlReg = CR Word8
  deriving (Eq, Ord)

instance Show ControlReg where
  show (CR w) = "cr" ++ show w

controlReg :: Word8 -> ControlReg
controlReg w = assert (w < 16) $ CR w

controlRegNo :: ControlReg -> Word8
controlRegNo (CR w) = w

------------------------------------------------------------------------
-- DebugReg

-- | There are 8 32-bit debug registers in ia32, and 16 64-bit
-- debug registers in ia64.
newtype DebugReg = DR Word8
  deriving (Eq, Ord)

instance Show DebugReg where
  show (DR w) = "dr" ++ show w

debugReg :: Word8 -> DebugReg
debugReg w = assert (w < 16) $ DR w

debugRegNo :: DebugReg -> Word8
debugRegNo (DR w) = w

------------------------------------------------------------------------
-- MMXReg

-- | There are 8 64-bit MMX registers
newtype MMXReg = MMXR Word8
  deriving (Eq, Ord)

instance Show MMXReg where
  show (MMXR w) = "mm" ++ show w

mmxReg :: Word8 -> MMXReg
mmxReg w = assert (w < 8) $ MMXR w

mmxRegNo :: MMXReg -> Word8
mmxRegNo (MMXR w) = w

mmxRegIdx :: MMXReg -> Int
mmxRegIdx = fromIntegral . mmxRegNo

------------------------------------------------------------------------
-- XMMReg

-- | There are 16 128-bit XMM registers
newtype XMMReg = XMMR Word8
  deriving (Eq, Ord)

instance Show XMMReg where
  show (XMMR w) = "xmm" ++ show w

xmmReg :: Word8 -> XMMReg
xmmReg w = assert (w < 16) $ XMMR w

xmmRegNo :: XMMReg -> Word8
xmmRegNo (XMMR w) = w

xmmRegIdx :: XMMReg -> Int
xmmRegIdx (XMMR w) = fromIntegral w

------------------------------------------------------------------------
-- YMMReg

-- | There are 16 128-bit XMM registers
newtype YMMReg = YMMR Word8
  deriving (Eq, Ord)

instance Show YMMReg where
  show (YMMR w) = "ymm" ++ show w

ymmReg :: Word8 -> YMMReg
ymmReg w = assert (w < 16) $ YMMR w

ymmRegNo :: YMMReg -> Word8
ymmRegNo (YMMR w) = w

ymmRegIdx :: YMMReg -> Int
ymmRegIdx (YMMR w) = fromIntegral w
