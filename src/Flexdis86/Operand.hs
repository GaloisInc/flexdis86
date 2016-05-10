{-# LANGUAGE TupleSections #-}
module Flexdis86.Operand (
  OperandType(..),
  OperandSize(..),
  OperandSource(..)
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
   | Opcode_reg Word8
     -- ^ A fixed register that is not affected by REX.
   | Reg_fixed Word8
     -- ^ An immediate value read in from the instruction stream.
   | ImmediateSource
     -- ^ An offset value read in from the instruction stream.
   | OffsetSource
     -- ^ A jump location that is read in from instruction stream, and
     -- offset from current instruction pointer.
   | JumpImmediate
  deriving (Eq, Show, Ord)

data OperandSize
   = BSize -- ^ Operand is always 8-bits.
   | WSize -- ^ Operand is always 16-bits.
   | DSize -- ^ Operand is always 32-bits.
   | QSize -- ^ Operand is always 64-bits.
   | VSize -- ^ Operand size equals operand mode (16,32 or 64 bits)
   | YSize -- ^ Operand size is 64-bits if operand size is 64 bits, and 32-bits otherwise.
   | ZSize -- ^ Operand size is 16-bits if operand size is 16 bits, and 32-bits otherwise.
   | RDQSize -- ^ Operand size is 64-bits on x64 and 32-bits on ia32.
  deriving (Eq, Show, Ord)

data OperandType
     -- | Operand that comes from a source and size.
   = OpType OperandSource OperandSize
     -- | A control register from ModRM.reg
   | RG_C
     -- | A debug register from ModRM.reg
   | RG_dbg
     -- | A segment register from ModRM.reg
   | RG_S
     -- | A floating point register index
   | RG_ST Int
     -- | An MMX register from ModRM.reg
   | RG_MMX_reg
     -- | A SSE XMM register from ModRM.reg
   | RG_XMM_reg
     -- | A SSE XMM register from ModRM.rm
   | RG_XMM_rm
     -- | A SSE XMM register or 64 bit address from ModRM.rm
   | RM_XMM
     -- | A specific segment
   | SEG Segment

     -- | Operand points to a far pointer in memory that may be 16,32,or 64 bits
     -- depending on operand size.
     -- Address stored from ModRM.rm (with ModRM.mod required to not equal 3).
   | M_FP

     -- | An address in memory with no defined target.
     -- Address stored from ModRM.rm (with ModRM.mod required to not equal 3).
   | M
     -- | A 64-bit integer memory location.
     -- Address stored from ModRM.rm (with ModRM.mod required to not equal 3).
   | M_X SizeConstraint

     -- | A 64-bit floating point memory location.
     -- Decoded as for M_X
   | M_FloatingPoint FPSizeConstraint

     -- | A register or memory location.
     -- As a register has size X-Size, and as memory has size X-Size.
     -- Stored in ModRM.rm
   | MXRX OperandSize OperandSize -- FIXME, should prob. be SizeConstraint

     -- | An MMX register or 64bit memory operand.
     -- Stored in ModRM.rm.
   | RM_MMX
     -- | An MMX register from ModRM.rm
   | RG_MMX_rm

     -- | An implicit memory location based upon the given register.
     -- Altered by ASO.
   | M_Implicit Segment Reg64 OperandSize

     -- | The constant one.
   | IM_1
    -- | An immediate with 8 bits that is sign extended to match operand size.
   | IM_SB
     -- | An immediate that is 32bits if REX.w set or operand size is 32 bits,
     -- and 16bits if operand size is 16bits.
     -- The value can be sign exected to match operator size.
   | IM_SZ
  deriving (Eq, Show, Ord)
