{-# LANGUAGE TupleSections #-}
module Flexdis86.Operand (
  OperandType(..),
  OperandSize(..),
  OperandSource(..),
  lookupOperandType,
  operandHandlerMap
  ) where

import qualified Data.Map as Map
import Data.Word ( Word8 )

import Flexdis86.OpTable
import Flexdis86.Register
import Flexdis86.Segment

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
  deriving (Eq, Show)

data OperandSize
   = BSize -- ^ Operand is always 8-bits.
   | WSize -- ^ Operand is always 16-bits.
   | DSize -- ^ Operand is always 32-bits.
   | QSize -- ^ Operand is always 64-bits.
   | VSize -- ^ Operand size equals operand mode (16,32 or 64 bits)
   | YSize -- ^ Operand size is 64-bits if operand size is 64 bits, and 32-bits otherwise.
   | ZSize -- ^ Operand size is 16-bits if operand size is 16 bits, and 32-bits otherwise.
   | RDQSize -- ^ Operand size is 64-bits on x64 and 32-bits on ia32.
  deriving (Eq, Show)

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
  deriving (Eq, Show)

operandHandlerMap :: Map.Map String OperandType
operandHandlerMap = Map.fromList
  [ -- Fixed values implicitly derived from opcode.
    (,) "AL"  $ OpType (Reg_fixed 0) BSize
  , (,) "eAX" $ OpType (Reg_fixed 0) ZSize
  , (,) "rAX" $ OpType (Reg_fixed 0) VSize
  , (,) "CL"  $ OpType (Reg_fixed 1) BSize
  , (,) "DX"  $ OpType (Reg_fixed 2) WSize

  , (,) "MIdb" $ M_Implicit es rdi BSize
  , (,) "MIdw" $ M_Implicit es rdi WSize
  , (,) "MIdd" $ M_Implicit es rdi DSize
  , (,) "MIdq" $ M_Implicit es rdi QSize  
  , (,) "MIsb" $ M_Implicit ds rsi BSize
  , (,) "MIsw" $ M_Implicit ds rsi WSize
  , (,) "MIsd" $ M_Implicit ds rsi DSize
  , (,) "MIsq" $ M_Implicit ds rsi QSize
    
    -- Fixed segment registers.
  , (,) "FS"  $ SEG fs
  , (,) "GS"  $ SEG gs

    -- Register values stored in opcode that also depend on REX.b
  , (,) "R0b" $ OpType (Opcode_reg 0) BSize
  , (,) "R1b" $ OpType (Opcode_reg 1) BSize
  , (,) "R2b" $ OpType (Opcode_reg 2) BSize
  , (,) "R3b" $ OpType (Opcode_reg 3) BSize
  , (,) "R4b" $ OpType (Opcode_reg 4) BSize
  , (,) "R5b" $ OpType (Opcode_reg 5) BSize
  , (,) "R6b" $ OpType (Opcode_reg 6) BSize
  , (,) "R7b" $ OpType (Opcode_reg 7) BSize
  , (,) "R0v" $ OpType (Opcode_reg 0) VSize
  , (,) "R1v" $ OpType (Opcode_reg 1) VSize
  , (,) "R2v" $ OpType (Opcode_reg 2) VSize
  , (,) "R3v" $ OpType (Opcode_reg 3) VSize
  , (,) "R4v" $ OpType (Opcode_reg 4) VSize
  , (,) "R5v" $ OpType (Opcode_reg 5) VSize
  , (,) "R6v" $ OpType (Opcode_reg 6) VSize
  , (,) "R7v" $ OpType (Opcode_reg 7) VSize
  , (,) "R0y" $ OpType (Opcode_reg 0) YSize
  , (,) "R1y" $ OpType (Opcode_reg 1) YSize
  , (,) "R2y" $ OpType (Opcode_reg 2) YSize
  , (,) "R3y" $ OpType (Opcode_reg 3) YSize
  , (,) "R4y" $ OpType (Opcode_reg 4) YSize
  , (,) "R5y" $ OpType (Opcode_reg 5) YSize
  , (,) "R6y" $ OpType (Opcode_reg 6) YSize
  , (,) "R7y" $ OpType (Opcode_reg 7) YSize

    -- Register values stored in ModRM.reg.
  , (,) "Gb"  $ OpType ModRM_reg BSize
  , (,) "Gd"  $ OpType ModRM_reg DSize
  , (,) "Gq"  $ OpType ModRM_reg QSize
  , (,) "Gv"  $ OpType ModRM_reg VSize
  , (,) "Gy"  $ OpType ModRM_reg YSize

    -- Control register read from ModRM.reg.
  , (,) "C"   $ RG_C
    -- Debug register read from ModRM.reg.
  , (,) "D"   $ RG_dbg

    -- MMX register stored in ModRM.reg
  , (,) "P"    $ RG_MMX_reg

    -- MMX register stored in ModRM.rm
    -- (ModRM.mod must equal 3).
  , (,) "N"    $ RG_MMX_rm
   -- Register operand stored in ModRM.rm (ModRM.mod must equal 3)
  , (,) "R"    $ OpType ModRM_rm_mod3 RDQSize

    -- RM fields are read from ModRM.rm
  , (,) "Eb"  $ OpType ModRM_rm BSize
  , (,) "Ew"  $ OpType ModRM_rm WSize
  , (,) "Ed"  $ OpType ModRM_rm DSize
  , (,) "Eq"  $ OpType ModRM_rm QSize
  , (,) "Ev"  $ OpType ModRM_rm VSize
  , (,) "Ey"  $ OpType ModRM_rm YSize

    -- Memory or register value stored in ModRM.rm
    -- As a  register has size VSize, and as memory has size WSize.
  , (,) "MwRv" $ MXRX VSize WSize
    -- As a  register has size DSize, and as memory has size BSize.
  , (,) "MbRd" $ MXRX BSize DSize
  , (,) "MwRy" $ MXRX WSize YSize

    -- Far Pointer (which is an address) stored in ModRM.rm
    -- (ModRM.mod must not equal 3)
  , (,) "Fv"   $ M_FP
    -- Memory value stored in ModRM.rm
    -- (ModRM.mod must not equal 3)
  , (,) "M"    $ M

    -- Memory value pointing to floating point value
  , (,) "M32fp" $ M_FloatingPoint FPSize32
  , (,) "M64fp" $ M_FloatingPoint FPSize64
  , (,) "M80fp" $ M_FloatingPoint FPSize80


    -- FP Register indicies
  , (,) "ST0"   $ RG_ST 0
  , (,) "ST1"   $ RG_ST 1
  , (,) "ST2"   $ RG_ST 2
  , (,) "ST3"   $ RG_ST 3
  , (,) "ST4"   $ RG_ST 4
  , (,) "ST5"   $ RG_ST 5
  , (,) "ST6"   $ RG_ST 6
  , (,) "ST7"   $ RG_ST 7

    -- Memory value pointing to 64-bit integer stored in ModRM.rm
    -- (ModRM.mod must not equal 3).
  , (,) "Mq"  $ M_X Size64
  , (,) "Md"  $ M_X Size32
  , (,) "Mw"  $ M_X Size16

  , (,) "S"   $ RG_S

  , (,) "Q"    $ RM_MMX

    -- Immediate values with different sizes.
    -- An immediate byte that does not need to be extended.
  , (,) "Ib"  $ OpType ImmediateSource BSize
    -- An immediate word that does not need to be extended.
  , (,) "Iw"  $ OpType ImmediateSource WSize
    -- A size v value that does not need to be extended.
  , (,) "Iv"  $ OpType ImmediateSource VSize
    -- A size z value that does not need to be extended.
  , (,) "Iz"  $ OpType ImmediateSource ZSize

    -- Constant one
  , (,) "I1"  $ IM_1

  , (,) "Jb"   $ OpType JumpImmediate  BSize
  , (,) "Jz"   $ OpType JumpImmediate  ZSize

  , (,) "Ob"   $ OpType OffsetSource BSize
  , (,) "Ov"   $ OpType OffsetSource VSize

    -- An immediate byte that is sign extended to an operator size.
  , (,) "sIb" $ IM_SB
    -- An immediate ZSize value that is sign extended to the operator
    -- size.
  , (,) "sIz" $ IM_SZ

    -- XMM
  , (,) "U"   $ RG_XMM_rm
  , (,) "V"   $ RG_XMM_reg
  , (,) "W"   $ RM_XMM
  ]

lookupOperandType :: String -> String -> OperandType
lookupOperandType i nm =
  case Map.lookup nm operandHandlerMap of
    Just h -> h
    Nothing -> error $ "Unknown operand: " ++ i ++ " " ++ show nm
