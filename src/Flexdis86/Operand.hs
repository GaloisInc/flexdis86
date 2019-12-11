{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2014-2016
Maintainer  : jhendrix@galois.com

This defines constants used for low-level operand layout information.
-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}
module Flexdis86.Operand
  ( OperandType(..)
  , OperandSize(..)
  , OperandSource(..)
  ) where

import Data.Word ( Word8 )

import Flexdis86.Register
import Flexdis86.Segment
import Flexdis86.Sizes

------------------------------------------------------------------------
-- OperandType

data OperandSource
     -- | Register or memory operand from ModRM.rm
   = ModRM_rm
     -- | Register operand stored with ModRM.rm (ModRM.mod must equal 3)
   | ModRM_rm_mod3
   | ModRM_reg -- ^ Register from ModRM.reg
     -- ^ Register whose index is derived from opcode.
     -- The word denote the index (0..7).
     -- rex.b is ored with this value to get the index of the
     -- Reg8.
   | Opcode_reg !Word8
     -- ^ A fixed register that is not affected by REX.
   | Reg_fixed !Word8
     -- ^ A register that is read from the VVVV field of the VEX prefix.
   | VVVV
     -- ^ An immediate value read in from the instruction stream.
   | ImmediateSource
     -- ^ An offset value read in from the instruction stream.
     --
     -- Values with the immedite source are not sign-extended like
     -- the IM_SB/IM_SZ type.
   | OffsetSource
     -- ^ A jump location that is read in from instruction stream, and
     -- offset from current instruction pointer.
   | JumpImmediate
  deriving (Eq, Show, Ord)

-- | The size of an operand in the udis86 file.
data OperandSize
   = BSize -- ^ Operand is always 8-bits.
   | WSize -- ^ Operand is always 16-bits.
   | DSize -- ^ Operand is always 32-bits.
   | QSize -- ^ Operand is always 64-bits.
   | OSize -- ^ Operand is always 128-bits.   (aka dq?)
   | QQSize  -- ^ Operand is always 256-bits.

   | VSize -- ^ Operand size equals operand mode (16,32 or 64 bits)
   | YSize -- ^ Operand size is 64-bits if operand size is 64 bits, and 32-bits otherwise.
   | ZSize -- ^ Operand size is 16-bits if operand size is 16 bits, and 32-bits otherwise.
   | RDQSize -- ^ Operand size is 64-bits on x64 and 32-bits on ia32.
  deriving (Eq, Show, Ord)

data OperandType
     -- | Operand that comes from a source and size.
   = OpType !OperandSource !OperandSize
     -- | A control register from ModRM.reg
   | RG_C
     -- | A debug register from ModRM.reg
   | RG_dbg
     -- | A segment register from ModRM.reg
   | RG_S
     -- | A floating point register index
   | RG_ST !Int
     -- | An MMX register from ModRM.reg
   | RG_MMX_reg


     -- | A SSE XMM register from ModRM.reg
   | RG_XMM_reg (Maybe OperandSize)

     -- | A SSE XMM/YMM register from ModRM.rm
   | RG_XMM_rm (Maybe OperandSize)

     -- | A SSE XMM/YMM register or 64 bit address from ModRM.rm
   | RM_XMM (Maybe OperandSize)

     -- | An XMM/YMM register from the VVVV field of the VEX prefix.
   | VVVV_XMM (Maybe OperandSize)

     -- | A specific segment
   | SEG !Segment

     -- | An absolute address encoded as a ptr16:16 or ptr16:32 depending on operand size
     --
     -- This is not used in 64-bit mode.
   | AbsoluteAddr

     -- | Operand points to a far pointer in memory that may be 16,32,or 64 bits
     -- depending on operand size.
     -- Address stored from ModRM.rm (with ModRM.mod required to not equal 3).
   | M_FP

     -- | An address in memory with no defined target.
     -- Address stored from ModRM.rm (with ModRM.mod required to not equal 3).
   | M

     -- | A memory location address whose value matches the given size.
     -- Address stored from ModRM.rm (with ModRM.mod required to not equal 3).
   | M_X !OperandSize

     -- | Either an XMM register or a memory location with the size determined from the operand size.
   | M_U !OperandSize

     -- | A 64-bit floating point memory location.
     -- Decoded as for M_X
   | M_FloatingPoint !FPSizeConstraint

     -- | A register or memory location.
     -- As a register has the first size, and as memory has size XySize.
     -- Stored in ModRM.rm
   | MXRX !OperandSize !OperandSize

     -- | An MMX register or 64bit memory operand.
     -- Stored in ModRM.rm.
   | RM_MMX
     -- | An MMX register from ModRM.rm
   | RG_MMX_rm

     -- | An implicit memory location based upon the given register.
     -- Altered by ASO.
   | M_Implicit !Segment !Reg64 !OperandSize

     -- | The constant one.
   | IM_1
     -- | An immediate with 8 bits that is sign extended to match operand size.
     --
     -- This will be decoded as an ByteImm.
   | IM_SB
     -- | An immediate that is 32bits if REX.w set or operand size is 32 bits,
     -- and 16bits if operand size is 16bits.
     -- The value can be sign exected to match operator size.
   | IM_SZ
  deriving (Eq, Ord, Show)
