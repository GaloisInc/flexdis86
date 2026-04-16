{- |
Module      :  $Header$
Copyright   :  (c) Galois, Inc 2013-2026
Maintainer  :  langston.barrett@gmail.com

Types describing x86 instruction definitions, and utilities for working
with them at runtime.  XML parsing lives in "Flexdis86.OpTable.Parse".
-}
{-# LANGUAGE DeriveGeneric #-}
module Flexdis86.OpTable
  ( -- * Primitive types
    Mode(..)
  , CPURequirement(..)
  , Vendor(..)
  , ModeLimit(..)
    -- * Def
  , Def(..)
  , defMnemonic
  , defMnemonicSynonyms
  , defVendor
  , defCPUReq
  , modeLimit
  , defSupported
  , defMode
  , reqAddrSize
  , OperandSizeConstraint(..)
  , reqOpSize
  , defPrefix
  , requiredPrefix
  , defOpcodes
  , requiredMod
  , requiredReg
  , requiredRM
  , x87ModRM
  , defOperands
  , x64Compatible
  , vexPrefixes
  , supportedCPUReqs
    -- * Operand lookup
  , lookupOperandType
  ) where

import qualified Control.DeepSeq as DS
import qualified Control.Monad.Fail as MF
import           Data.Bits ((.&.))
import           Data.Binary (Binary)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Lens.Micro (Lens', lens, (^.))

import           Flexdis86.Operand
import           Flexdis86.Register
import           Flexdis86.Segment
import           Flexdis86.Sizes

------------------------------------------------------------------------
-- Mode

-- | Mode effect on instruction semantics.
data Mode
     -- | Default operand size is 64 bits in x64 mode.
   = Default64
     -- | Instruction is invalid in x64 mode.
   | Invalid64
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData Mode
-- | For "Flexdis86.OpTable.Parse".
instance Binary Mode

------------------------------------------------------------------------
-- CPURequirement

-- | Defines which features are required for instruction to be supported.
data CPURequirement
     -- | This is a base instruction.
   = Base
     -- | This uses the X87 architecture.
   | X87
     -- | This is an undocumented X86 instruction.
   | X87_undocumented
     -- | Safer mode extensions
   | SMX
     -- | AMD 3D now definition.
   | AMD_3DNOW
   | SSE
   | SSE2
   | SSE3
     -- | The movbe instruction which is unique to the atom.
   | SSE3_atom
   | SSE4_1
   | SSE4_2
     -- | AES new instructions.
   | AESNI
     -- | SHA extension
   | SHA
   | AVX    -- ^ Advanced vector extensions
   | BMI2   -- ^ Bit manipulation instructions
   | ADX    -- ^ Multi-precision add-carry instructions
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData CPURequirement
-- | For "Flexdis86.OpTable.Parse".
instance Binary CPURequirement

------------------------------------------------------------------------
-- Vendor

-- | Defines whether instruction is vendor specific.
data Vendor = AMD | Intel
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData Vendor
-- | For "Flexdis86.OpTable.Parse".
instance Binary Vendor

------------------------------------------------------------------------
-- ModeLimit

-- | Indicates restrictions on which mode instruction may run in.
data ModeLimit
   = AnyMode
   | Only32
   | Only64
   | Not64
  deriving (Eq, Generic, Show)

instance DS.NFData ModeLimit
-- | For "Flexdis86.OpTable.Parse".
instance Binary ModeLimit

valid64 :: ModeLimit -> Bool
valid64 m = m == AnyMode || m == Only64

------------------------------------------------------------------------
-- OperandSizeConstraint / Def

data OperandSizeConstraint
   = OpSize16
   | OpSize32
   | OpSize64
  deriving (Eq, Generic, Show)

instance DS.NFData OperandSizeConstraint
-- | For "Flexdis86.OpTable.Parse".
instance Binary OperandSizeConstraint

-- | The definition of an instruction.
data Def = Def  { _defMnemonic         :: !BS.ByteString
                  -- ^ Canonical mnemonic
                , _defMnemonicSynonyms :: [BS.ByteString]
                  -- ^ Additional mnemonics, not including
                  -- the canonical mnemonic. Used e.g. by
                  -- jump instructions.
                , _defCPUReq :: CPURequirement
                  -- ^ XXX: we should be able to combine these.
                  -- For example, instruction `vaesenc` requires both
                  -- AES and AVX support.
                , _defVendor :: Maybe Vendor
                , _modeLimit :: ModeLimit
                , _defMode   :: Maybe Mode
                , _reqAddrSize :: Maybe SizeConstraint
                , _reqOpSize :: Maybe OperandSizeConstraint
                , _defPrefix :: [String]
                  -- ^ List of allowed prefixes.
                , _requiredPrefix :: Maybe Word8
                , _defOpcodes :: [Word8]
                  -- ^ List of opcodes, which should be nonempty for
                  -- a complete 'Def'.
                , _requiredMod :: Maybe ModConstraint
                , _requiredReg :: {-# UNPACK #-} !MaybeFin8
                , _requiredRM  :: {-# UNPACK #-} !MaybeFin8
                , _x87ModRM    :: Maybe Fin64
                , _vexPrefixes  :: ![ [Word8] ]
                  -- ^ Allowed VEX prefixes for this instruction.
                , _defOperands  :: ![OperandType]
                } deriving (Eq, Generic, Show)

instance DS.NFData Def
-- | For "Flexdis86.OpTable.Parse".
instance Binary Def

------------------------------------------------------------------------
-- Def lenses

-- | Canonical mnemonic for definition.
defMnemonic :: Lens' Def BS.ByteString
defMnemonic = lens _defMnemonic (\s v -> s { _defMnemonic = v })

-- | Additional mnemonics, not including the canonical mnemonic.
--
-- Used e.g. by jump instructions.
defMnemonicSynonyms :: Lens' Def [BS.ByteString]
defMnemonicSynonyms =
  lens _defMnemonicSynonyms (\s v -> s { _defMnemonicSynonyms = v })

-- | CPU requirements on the definition.
defCPUReq :: Lens' Def CPURequirement
defCPUReq = lens _defCPUReq (\s v -> s { _defCPUReq = v })

-- | Vendor requirements on the definition.
defVendor :: Lens' Def (Maybe Vendor)
defVendor = lens _defVendor (\s v -> s { _defVendor = v })

-- | Restrictions on the mode of the CPU.
modeLimit :: Lens' Def ModeLimit
modeLimit = lens _modeLimit (\s v -> s { _modeLimit = v })

-- | Modifications to x64 mode.
defMode :: Lens' Def (Maybe Mode)
defMode = lens _defMode (\s v -> s { _defMode = v })

-- | Expected address size for instruction.
reqAddrSize :: Lens' Def (Maybe SizeConstraint)
reqAddrSize = lens _reqAddrSize (\s v -> s { _reqAddrSize = v })

-- | Expected operand size for instruction.
reqOpSize :: Lens' Def (Maybe OperandSizeConstraint)
reqOpSize = lens _reqOpSize (\s v -> s { _reqOpSize = v })

-- | Prefixes allowed on instruction.
defPrefix :: Lens' Def [String]
defPrefix = lens _defPrefix (\s v -> s { _defPrefix = v })

-- | Prefixe required by an instruction, if any.
requiredPrefix :: Lens' Def (Maybe Word8)
requiredPrefix = lens _requiredPrefix (\s v -> s { _requiredPrefix = v })

-- | Opcodes on instruction. This should be nonempty for a complete 'Def'.
defOpcodes :: Lens' Def [Word8]
defOpcodes = lens _defOpcodes (\s v -> s { _defOpcodes = v })

-- | Constraint on the modRM.mod value.
requiredMod :: Lens' Def (Maybe ModConstraint)
requiredMod = lens _requiredMod (\s v -> s { _requiredMod = v })

-- | Indicates if instruction must have ModR/M value with the
-- given value in the reg field.
requiredReg :: Lens' Def MaybeFin8
requiredReg = lens _requiredReg (\s v -> s { _requiredReg = v })

-- | Indicates if instruction must have ModR/M value with the
-- given value in the rm field.
requiredRM :: Lens' Def MaybeFin8
requiredRM = lens _requiredRM (\s v -> s { _requiredRM = v })

-- | An x87 FPU opcode expected in the low 6-bits of a ModRM byte
-- following instruction.
x87ModRM :: Lens' Def (Maybe Fin64)
x87ModRM = lens _x87ModRM (\s v -> s { _x87ModRM = v })

vexPrefixes :: Lens' Def [[Word8]]
vexPrefixes = lens _vexPrefixes (\s v -> s { _vexPrefixes = v })

-- | Operand descriptions.
defOperands :: Lens' Def [OperandType]
defOperands = lens _defOperands (\s v -> s { _defOperands = v })

------------------------------------------------------------------------
-- Filtering

-- | CPU requirements accepted by flexdis86.
supportedCPUReqs :: [CPURequirement]
supportedCPUReqs =
  [Base, SSE, SSE2, SSE3, SSE3_atom, SSE4_1, SSE4_2,
   X87, AESNI, SHA, AVX, BMI2, ADX]

-- | Return true if this instruction is compatible with 64-bit mode.
x64Compatible :: Def -> Bool
x64Compatible d =
  case d^.defOpcodes of
    [b] | null (d^.vexPrefixes) && (b .&. 0xF0 == 0x40) -> False
    _   -> valid64 (d^.modeLimit)

-- | Return true if this definition is one supported by flexdis86.
defSupported :: Def -> Bool
defSupported d = d^.reqAddrSize /= Just Size16
              && d^.defCPUReq `elem` supportedCPUReqs
              && x64Compatible d

------------------------------------------------------------------------
-- Operand lookup

operandHandlerMap :: Map.Map String OperandType
operandHandlerMap = Map.fromList
  [ -- Fixed values implicitly derived from opcode.
    (,) "AL"  $ OpType (Reg_fixed 0) BSize
  , (,) "AX"  $ OpType (Reg_fixed 0) WSize
  , (,) "eAX" $ OpType (Reg_fixed 0) ZSize
  , (,) "Av"  $ AbsoluteAddr
  , (,) "rAX" $ OpType (Reg_fixed 0) VSize
  , (,) "CL"  $ OpType (Reg_fixed 1) BSize
  , (,) "DX"  $ OpType (Reg_fixed 2) WSize

  , (,) "MIdb" $ M_Implicit ES RDI BSize
  , (,) "MIdw" $ M_Implicit ES RDI WSize
  , (,) "MIdd" $ M_Implicit ES RDI DSize
  , (,) "MIdq" $ M_Implicit ES RDI QSize
  , (,) "MIsb" $ M_Implicit DS RSI BSize
  , (,) "MIsw" $ M_Implicit DS RSI WSize
  , (,) "MIsd" $ M_Implicit DS RSI DSize
  , (,) "MIsq" $ M_Implicit DS RSI QSize

    -- Fixed segment registers.
  , (,) "CS"  $ SEG CS
  , (,) "DS"  $ SEG DS
  , (,) "ES"  $ SEG ES
  , (,) "FS"  $ SEG FS
  , (,) "GS"  $ SEG GS
  , (,) "SS"  $ SEG SS

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

  , (,) "R0z" $ OpType (Opcode_reg 0) ZSize
  , (,) "R1z" $ OpType (Opcode_reg 1) ZSize
  , (,) "R2z" $ OpType (Opcode_reg 2) ZSize
  , (,) "R3z" $ OpType (Opcode_reg 3) ZSize
  , (,) "R4z" $ OpType (Opcode_reg 4) ZSize
  , (,) "R5z" $ OpType (Opcode_reg 5) ZSize
  , (,) "R6z" $ OpType (Opcode_reg 6) ZSize
  , (,) "R7z" $ OpType (Opcode_reg 7) ZSize

    -- Register values stored in ModRM.reg.
  , (,) "Gb"  $ OpType ModRM_reg BSize
  , (,) "Gd"  $ OpType ModRM_reg DSize
  , (,) "Gq"  $ OpType ModRM_reg QSize
  , (,) "Gv"  $ OpType ModRM_reg VSize
  , (,) "Gw"  $ OpType ModRM_reg WSize
  , (,) "Gy"  $ OpType ModRM_reg YSize

    -- Control register read from ModRM.reg.
  , (,) "C"   $ RG_C
    -- Debug register read from ModRM.reg.
  , (,) "D"   $ RG_dbg

    -- MMX register stored in ModRM.reg
  , (,) "P"    $ RG_MMX_reg

    -- MMX register stored in ModRM.rm (ModRM.mod must equal 3).
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
  , (,) "MwRv" $ MXRX WSize VSize
    -- As a  register has size DSize, and as memory has size BSize.
  , (,) "MbRd" $ MXRX BSize DSize
  , (,) "MbRv" $ MXRX BSize VSize
  , (,) "MdRy" $ MXRX DSize YSize
  , (,) "MwRd" $ MXRX WSize DSize
  , (,) "MwRy" $ MXRX WSize YSize

    -- Far Pointer stored in ModRM.rm (ModRM.mod must not equal 3)
  , (,) "Fv"   $ M_FP
    -- Memory value stored in ModRM.rm (ModRM.mod must not equal 3)
  , (,) "M"    $ M
  , (,) "Mo"   $ M_X OSize
    -- A reference to an 80-bit floating point value
  , (,) "Mt"   $ M_FloatingPoint FPSize80

    -- Memory value pointing to floating point value
  , (,) "M32fp" $ M_FloatingPoint FPSize32
  , (,) "M64fp" $ M_FloatingPoint FPSize64

    -- FP Register indices
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
  , (,) "Mq"  $ M_X QSize
  , (,) "Md"  $ M_X DSize
  , (,) "Mv"  $ M_X VSize
  , (,) "Mw"  $ M_X WSize
  , (,) "Mo"  $ M_X OSize

  , (,) "MdU"  $ M_U DSize
  , (,) "MqU"  $ M_U QSize
  , (,) "MwU"  $ M_U WSize

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
    -- An immediate ZSize value that is sign extended to the operator size.
  , (,) "sIz" $ IM_SZ

    -- XMM
  , (,) "U"   $ RG_XMM_rm Nothing
  , (,) "Ux"  $ RG_XMM_rm Nothing
  , (,) "Udq" $ RG_XMM_rm (Just OSize)
  , (,) "Uqq" $ RG_XMM_rm (Just QQSize)

  , (,) "V"   $ RG_XMM_reg Nothing
  , (,) "Vx"  $ RG_XMM_reg Nothing
  , (,) "Vdq" $ RG_XMM_reg (Just OSize)
  , (,) "Vqq" $ RG_XMM_reg (Just QQSize)

  , (,) "W"   $ RM_XMM Nothing
  , (,) "Wx"  $ RM_XMM Nothing
  , (,) "Wdq" $ RM_XMM (Just OSize)
  , (,) "Wqq" $ RM_XMM (Just QQSize)

  , (,) "Hd"  $ OpType VVVV DSize
  , (,) "Hq"  $ OpType VVVV QSize
  , (,) "Hx"  $ VVVV_XMM Nothing
  , (,) "Hdq" $ VVVV_XMM (Just OSize)
  , (,) "Hqq" $ VVVV_XMM (Just QQSize)
  ]

lookupOperandType :: MF.MonadFail m => BS.ByteString -> String -> m OperandType
lookupOperandType i nm =
  case Map.lookup nm operandHandlerMap of
    Just h  -> pure h
    Nothing -> fail $ "Unknown operand for " ++ BSC.unpack i ++ " named " ++ show nm
