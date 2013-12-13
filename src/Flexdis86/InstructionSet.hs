{- |
Module      :  $Header$
Description :  Declares datatypes for the instruction set.
Copyright   :  (c) Galois, Inc 2013
Maintainer  :  jhendrix@galois.com

This declares the main datatypes for the instruction set.
-}
module Flexdis86.InstructionSet
  ( InstructionInstance(..)
  , Value(..)
  , ControlReg, controlReg, controlRegNo
  , DebugReg, debugReg, debugRegNo
  , MMXReg, mmx_reg {- deprecated -}, mmxReg, mmxRegNo
  , XMMReg, xmmReg, xmmRegNo
  , LockPrefix(..)
  , Segment, es, cs, ss, ds, fs, gs, segmentRegisterByIndex
  , AddrRef(..)
  , Word8
  , Word16
  , Word32
  , Word64
  , Reg8, low_reg8, high_reg8, al, bl, cl, dl, ah, bh, ch, dh
  , Reg16, reg16, ax, bx, cx, dx
  , Reg32, reg32, eax, ebx, ecx, edx, esp, ebp, esi, edi
  , Reg64, reg64, reg64Idx, rax, rbx, rcx, rdx, rsp, rbp, rsi, rdi
  , Int64
  ) where 

import Control.Exception
import Data.Int
import Data.Word
import qualified Data.Vector as V

-- | There are 16 control registers CR0 through CR15.  
newtype ControlReg = CR Word8
                     deriving Eq
                     
instance Show ControlReg where
  show (CR w) = "CR" ++ show w

controlReg :: Word8 -> ControlReg
controlReg w = assert (w < 16) $ CR w

controlRegNo :: ControlReg -> Word8
controlRegNo (CR w) = w

-- | There are 8 32-bit debug registers in ia32, and 16 64-bit
-- debug registers in ia64.
newtype DebugReg   = DR Word8
  deriving (Show, Eq)

debugReg :: Word8 -> DebugReg
debugReg w = assert (w < 16) $ DR w

debugRegNo :: DebugReg -> Word8 
debugRegNo (DR w) = w

-- | There are 8 64-bit MMX registers
newtype MMXReg = MMXR Word8
  deriving (Show, Eq)

{-# DEPRECATED mmx_reg "Use mmxReg instead!" #-}
mmx_reg :: Word8 -> MMXReg
mmx_reg w = assert (w < 8) $ MMXR w

mmxReg :: Word8 -> MMXReg
mmxReg w = assert (w < 8) $ MMXR w

mmxRegNo :: MMXReg -> Word8
mmxRegNo (MMXR w) = w

-- | There are 16 128-bit XMM registers
newtype XMMReg = XMMR Word8
  deriving (Show, Eq)

xmmReg :: Word8 -> XMMReg
xmmReg w = assert (w < 16) $ XMMR w

xmmRegNo :: XMMReg -> Word8
xmmRegNo (XMMR w) = w

------------------------------------------------------------------------
-- Reg8

-- | We use 0 to 15 to correspond to denote the low-order bytes
-- of the registers, and 16-19 to denote bits 8-15 of regists
-- rax, rcx, rdx, and rbx respectively.
data Reg8 = Reg8 Word8
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
al = low_reg8 (unReg64 rax)

bl :: Reg8
bl = low_reg8 (unReg64 rbx)

cl :: Reg8
cl = low_reg8 (unReg64 rcx)

dl :: Reg8
dl = low_reg8 (unReg64 rdx)

ah :: Reg8
ah = high_reg8 (unReg64 rax)

bh :: Reg8
bh = high_reg8 (unReg64 rbx)

ch :: Reg8
ch = high_reg8 (unReg64 rcx)

dh :: Reg8
dh = high_reg8 (unReg64 rdx)

------------------------------------------------------------------------
-- Reg16

-- | We always get the low order 16-bits of a 64-bit register.
newtype Reg16 = Reg16 Word8
  deriving (Eq, Ord)

reg16 :: Word8 -> Reg16
reg16 i = assert (i < 16) (Reg16 i)

instance Show Reg16 where
  show (Reg16 i) = assert (i < 16) (regNames16 V.! fromIntegral i)

regNames16 :: V.Vector String
regNames16 = V.fromList [ "ax",   "cx",   "dx",   "bx"
                        , "sp",   "bp",   "si",   "di"
                        , "r8w" , "r9w" , "r10w", "r11w"
                        , "r12w", "r13w", "r14w", "r15w"
                        ]
  
ax :: Reg16
ax = Reg16 (unReg64 rax)

bx :: Reg16
bx = Reg16 (unReg64 rbx)

cx :: Reg16
cx = Reg16 (unReg64 rcx)

dx :: Reg16
dx = Reg16 (unReg64 rdx)

------------------------------------------------------------------------
-- Reg32

-- | We always get the low order 32-bits of a 64-bit register.
newtype Reg32 = Reg32 Word8
  deriving (Eq, Ord)

reg32 :: Word8 -> Reg32
reg32 i = assert (i < 16) $ Reg32 i


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

rax :: Reg64
rax = Reg64 0

rcx :: Reg64
rcx = Reg64 1

rdx :: Reg64
rdx = Reg64 2

rbx :: Reg64
rbx = Reg64 3

rsp :: Reg64
rsp = Reg64 4

rbp :: Reg64
rbp = Reg64 5

rsi :: Reg64
rsi = Reg64 6

rdi :: Reg64
rdi = Reg64 7

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

------------------------------------------------------------------------
-- AddrRef

data AddrRef
    -- | @Addr_32 s b i o@ denotes a 32-bitIP address that will 
    -- be zero extended in segment @s@ with base @b@, index @i@, and offset @o@.
  = Addr_32      Segment (Maybe Reg32) (Maybe (Int, Reg32)) Int32
  | IP_Offset_32 Segment Int32
    -- | Offset relative to segment base.
  | Offset_32    Segment Word32
    -- | Offset relative to segment base.
  | Offset_64    Segment Word64
  | Addr_64      Segment (Maybe Reg64) (Maybe (Int, Reg64)) Int32
  | IP_Offset_64 Segment Int32
  deriving (Show, Eq)

------------------------------------------------------------------------
-- Value

-- | The value of an operand in an instruction instance.
data Value
  = ControlReg ControlReg
  | DebugReg DebugReg
  | MMXReg MMXReg
  | XMMReg XMMReg  
  | SegmentValue Segment
  | FarPointer AddrRef
  | VoidMem AddrRef
  | Mem8  AddrRef
  | Mem16 AddrRef
  | Mem32 AddrRef
  | Mem64 AddrRef
  | ByteImm  Word8
  | WordImm  Word16
  | DWordImm Word32
  | QWordImm Word64
  | ByteReg  Reg8
  | WordReg  Reg16
  | DWordReg Reg32
  | QWordReg Reg64
  | JumpOffset Int64
  deriving (Show, Eq)

------------------------------------------------------------------------
-- InstructionInstance

data LockPrefix
   = NoLockPrefix
   | RepPrefix
   | RepZPrefix
  deriving (Show, Eq)

-- | Instruction instance with name and operands.
data InstructionInstance 
   = II { iiLockPrefix :: LockPrefix
        , iiOp :: String
        , iiArgs :: [Value]
        }
  deriving (Show, Eq)
