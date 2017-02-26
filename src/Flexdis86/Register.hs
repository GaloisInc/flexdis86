{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2014-2016
Maintainer  : jhendrix@galois.com

Defines types for x86 registers.
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
module Flexdis86.Register (
    -- * 8-bit General Purpose registers
    Reg8(..), low_reg8, high_reg8, al, bl, cl, dl, ah, bh, ch, dh, is_low_reg, is_high_reg
    -- * 16-bit General Purpose registers
  , Reg16(..), reg16, ax, bx, cx, dx, reg16_reg
    -- * 32-bit General Purpose registers
  , Reg32(..), reg32, eax, ebx, ecx, edx, esp, ebp, esi, edi, reg32_reg

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
  ) where

import           Control.Exception ( assert )
import qualified Data.Vector as V
import           Data.Word ( Word8 )

-- | There are 16 control registers CR0 through CR15.
newtype ControlReg = CR Word8
  deriving (Eq, Ord)

instance Show ControlReg where
  show (CR w) = "cr" ++ show w

controlReg :: Word8 -> ControlReg
controlReg w = assert (w < 16) $ CR w

controlRegNo :: ControlReg -> Word8
controlRegNo (CR w) = w

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
-- Reg8

-- | We use 0 to 15 to correspond to denote the low-order bytes
-- of the registers, and 16-19 to denote bits 8-15 of regists
-- rax, rcx, rdx, and rbx respectively.
newtype Reg8 = Reg8 Word8
  deriving (Eq, Ord)

instance Show Reg8 where
  show (Reg8 i) = assert (i < 20) (regNames8 V.! (fromIntegral i))

regNames8 :: V.Vector String
regNames8 = V.fromList [ "al",   "cl",   "dl",   "bl"
                       , "spl",  "bpl",  "sil",  "dil"
                       , "r8b" , "r9b" , "r10b", "r11b"
                       , "r12b", "r13b", "r14b", "r15b"
                       , "ah",   "ch",   "dh",   "bh"
                       ]

low_reg8 :: Word8 -> Reg8
low_reg8 w = assert (w < 16) $ Reg8 w

-- | Returns the high register ah, ch, dh, or bh from the index value.
high_reg8 :: Word8 -> Reg8
high_reg8 w = assert (w < 4) $ Reg8 $ 16+w

al :: Reg8
al = low_reg8 (unReg64 RAX)

bl :: Reg8
bl = low_reg8 (unReg64 RBX)

cl :: Reg8
cl = low_reg8 (unReg64 RCX)

dl :: Reg8
dl = low_reg8 (unReg64 RDX)

ah :: Reg8
ah = high_reg8 (unReg64 RAX)

bh :: Reg8
bh = high_reg8 (unReg64 RBX)

ch :: Reg8
ch = high_reg8 (unReg64 RCX)

dh :: Reg8
dh = high_reg8 (unReg64 RDX)

is_low_reg  :: Reg8 -> Maybe Reg64
is_low_reg (Reg8 r)
  | r < 16    = return $ Reg64 r
  | otherwise = Nothing

is_high_reg :: Reg8 -> Maybe Reg64
is_high_reg (Reg8 r)
  | 16 <= r && r <= 19   = return $ Reg64 (r - 16)
  | otherwise            = Nothing

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

ax :: Reg16
ax = Reg16 (unReg64 RAX)

bx :: Reg16
bx = Reg16 (unReg64 RBX)

cx :: Reg16
cx = Reg16 (unReg64 RCX)

dx :: Reg16
dx = Reg16 (unReg64 RDX)

------------------------------------------------------------------------
-- Reg32

-- | We always get the low order 32-bits of a 64-bit register.
newtype Reg32 = Reg32 Word8
  deriving (Eq, Ord)

reg32 :: Word8 -> Reg32
reg32 i = assert (i < 16) $ Reg32 i

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

eax :: Reg32
eax = Reg32 0


ecx :: Reg32
ecx = Reg32 1

edx :: Reg32
edx = Reg32 2

ebx :: Reg32
ebx = Reg32 3

esp :: Reg32
esp = Reg32 4

ebp :: Reg32
ebp = Reg32 5

esi :: Reg32
esi = Reg32 6

edi :: Reg32
edi = Reg32 7

------------------------------------------------------------------------
-- Reg64

newtype Reg64 = Reg64 { unReg64 :: Word8 }
  deriving (Eq, Ord)

-- | Make reg64 by index.
reg64 :: Word8 -> Reg64
reg64 i = assert (i < 16) $ Reg64 i

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
