{- |
Module      :  $Header$
Description :  Declares datatypes for the instruction set.
Copyright   :  (c) Galois, Inc 2013
Maintainer  :  jhendrix@galois.com

This declares the main datatypes for the instruction set.
-}
{-# LANGUAGE PatternSynonyms #-}
module Flexdis86.InstructionSet
  ( InstructionInstance
  , InstructionInstanceF(..)
  , ppInstruction
  , instructionSize
  , SizeConstraint(..)
  , OperandSize(..)
  , OperandType(..)
  , OperandSource(..)
  , Value(..)
  , ControlReg, controlReg, controlRegNo
  , DebugReg, debugReg, debugRegNo
  , MMXReg, mmxReg, mmxRegNo, mmxRegIdx
  , XMMReg, xmmReg, xmmRegNo, xmmRegIdx
  , LockPrefix(..), ppLockPrefix
    -- * Synonyms
  , Segment
  , pattern ES
  , pattern CS
  , pattern SS
  , pattern DS
  , pattern FS
  , pattern GS
  , segmentRegisterByIndex
  , segmentRegNo
  , Displacement(..)
  , AddrRef(..)
  , Word8
  , Word16
  , Word32
  , Word64
  , Int64
  ) where

import Control.Applicative
import Data.Int
import Data.Word
import Numeric (showHex)
import Text.PrettyPrint.ANSI.Leijen hiding (empty, (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Prelude

import Flexdis86.Operand
import Flexdis86.Sizes
import Flexdis86.Prefixes
import Flexdis86.Register
import Flexdis86.Segment

------------------------------------------------------------------------
-- AddrRef

-- | Displacement in an indirect memory reference.
--
-- These can be encoded as either 8 or 32 bits
data Displacement = Disp32 Int32
                  | Disp8 Int8
                  | NoDisplacement
                  deriving (Eq, Ord, Show)

displacementInt :: Displacement -> Int
displacementInt d =
  case d of
    NoDisplacement -> 0
    Disp32 o -> fromIntegral o
    Disp8 o -> fromIntegral o

data AddrRef
  = Addr_32      Segment (Maybe Reg32) (Maybe (Int, Reg32)) Displacement
  -- ^ @Addr_32 s b i o@ denotes a 32-bit IP address that will be
  -- zero extended in segment @s@ with base @b@, index @i@, and
  -- offset @o@.  The index is the offset register along with a
  -- scalar multiplier.
  --
  -- In GNU syntax this is displacement(base register, offset
  -- register, scalar multiplier).
  --
  -- In Intel syntax, this is the much more sensible [base register
  -- + displacement + offset register * scalar multiplier]
  | IP_Offset_32 Segment Displacement
    -- | Offset relative to segment base.
  | Offset_32    Segment Word32
    -- | Offset relative to segment base.
  | Offset_64    Segment Word64
  | Addr_64      Segment (Maybe Reg64) (Maybe (Int, Reg64)) Displacement
  | IP_Offset_64 Segment Displacement
  deriving (Show, Eq, Ord)

pp0xHex :: (Integral a, Show a) => a -> Doc
pp0xHex n = text "0x" <> text (showHex n' "")
  where n' = fromIntegral (fromIntegral n :: Int64) :: Word64

ppSigned0xHex :: (Integral a, Show a) => a -> Doc
ppSigned0xHex n = sign_ <> pp0xHex (abs n)
  where sign_ = if n >= 0 then PP.empty else text "-"

ppAddrRef :: AddrRef -> Doc
ppAddrRef addr =
  case addr of
    Addr_32 seg base roff off ->
       case base of
         Just r | isDefaultSeg32 seg r -> a
                | seg == FS -> text (show seg) <> colon <+> a
                | seg == GS -> text (show seg) <> colon <+> a
                | otherwise -> a -- ((text (show seg) <> colon) <+>)
         _ -> a
      where a = ppAddr seg base roff off
                                                          -- or rip? this is 32 bits ...
    IP_Offset_32 seg off      -> brackets $ text "ip+" <> pp0xHex (displacementInt off)
    Offset_32 seg off         -> prefix seg off
    Offset_64 seg off         -> prefix seg off
    Addr_64 seg base roff off ->
      case base of
        Just r  | seg == FS -> text (show seg) <> colon <> a
                | seg == GS -> text (show seg) <> colon <> a
                | isDefaultSeg64 seg r -> a
                | otherwise -> a
        Nothing | seg == FS -> text (show seg) <> colon <> a
                | seg == GS -> text (show seg) <> colon <> a
	        | otherwise -> a
      where a = ppAddr seg base roff off
    Addr_64 seg base roff off ->
      (case base
         of Just r  | seg == FS -> ((text (show seg) <> colon) <>)
                    | seg == GS -> ((text (show seg) <> colon) <>)
                    | isDefaultSeg64 seg r -> id
                    | otherwise -> id -- ((text (show seg) <> colon) <+>)
            Nothing | seg == FS -> ((text (show seg) <> colon) <>)
                    | seg == GS -> ((text (show seg) <> colon) <>)
	            | otherwise -> id) (ppAddr seg base roff off)
    IP_Offset_64 seg off      -> brackets $ text "rip+"<> pp0xHex (displacementInt off)
  where
    prefix seg off = ppShowReg seg <> colon <> text (show off)
    addOrSub n = text $ if n >= 0 then "+" else "-"
    ppAddr seg base roff off =
      case (base, roff, displacementInt off) of
         (Nothing, Nothing, 0)      -> text "0x0" -- happens with fs and gs segments
         (Nothing, Just (n, r), 0)  -> brackets (ppShowReg r <> text "*" <> int n)
         (Just r, Nothing, 0)      -> brackets (ppShowReg r)
         (Just r, Just (n, r'), 0)  -> brackets (ppShowReg r <> text "+" <> ppShowReg r' <> text "*" <> int n)
         (Nothing, Nothing, n')     -> pp0xHex n'
         (Just r, Nothing, n')      -> brackets (ppShowReg r <> addOrSub n' <>  pp0xHex (abs n'))
         (Nothing, Just (n, r), n') ->
           brackets (ppShowReg r <> text "*" <> int n <> addOrSub n' <> pp0xHex (abs n'))
         (Just r, Just (n, r'), n') ->
           brackets (ppShowReg r <> text "+" <> ppShowReg r' <> text "*" <> int n <> addOrSub (fromIntegral n') <> pp0xHex (abs n'))



------------------------------------------------------------------------
-- Value

{-
data ValueClass = Address
                | Immediate
                | GPR
-}

-- | The value of an operand in an instruction instance.
data Value
  = ControlReg ControlReg
  | DebugReg DebugReg
  | MMXReg MMXReg
  | XMMReg XMMReg
  | SegmentValue Segment
  | X87Register Int
  | FarPointer AddrRef
  | VoidMem AddrRef
  | Mem8  AddrRef
  | Mem16 AddrRef
  | Mem32 AddrRef
  | Mem64 AddrRef
  | Mem128 AddrRef
  | FPMem32 AddrRef
  | FPMem64 AddrRef
  | FPMem80 AddrRef
  | ByteImm  Word8
  | WordImm  Word16
  | DWordImm Word32
  | QWordImm Word64
  | ByteReg  Reg8
  | WordReg  Reg16
  | DWordReg Reg32
  | QWordReg Reg64
  | JumpOffset OperandSize Int64
  deriving (Show, Eq, Ord)

ppShowReg :: Show r => r -> Doc
ppShowReg r = text (show r)

ppValue :: Word64 -- ^ Base address for offset printing.
                  -- This should be the address of the next instruction.
        -> Value
        -> Doc
ppValue base v =
  case v of
    ControlReg   r    -> text (show r)
    DebugReg     r    -> text (show r)
    MMXReg       r    -> text (show r)
    XMMReg       r    -> text (show r)
    X87Register  n    -> text "st" <> if n == 0 then PP.empty else parens (int n)
    SegmentValue r    -> ppShowReg    r
    -- do the "*" belong here or in ppAddrRef?
    FarPointer   addr -> text "??FAR PTR??"            <+> ppAddrRef    addr
    VoidMem      addr -> ppAddrRef addr
    Mem8         addr -> text "BYTE PTR"    <+> ppAddrRef addr
    Mem16        addr -> text "WORD PTR"    <+> ppAddrRef addr
    Mem32        addr -> text "DWORD PTR"   <+> ppAddrRef addr
    Mem64        addr -> text "QWORD PTR"   <+> ppAddrRef addr
    Mem128       addr -> text "XMMWORD PTR" <+> ppAddrRef addr
    FPMem32      addr -> text "DWORD PTR"   <+> ppAddrRef addr
    FPMem64      addr -> text "QWORD PTR"   <+> ppAddrRef addr
    FPMem80      addr -> text "TBYTE PTR"   <+> ppAddrRef addr
    ByteImm      imm  -> ppImm imm
    WordImm      imm  -> ppImm imm
    DWordImm     imm  -> ppImm imm
    QWordImm     imm  -> ppImm imm
    ByteReg      r    -> ppShowReg    r
    WordReg      r    -> ppShowReg    r
    DWordReg     r    -> ppShowReg    r
    QWordReg     r    -> ppShowReg    r
    JumpOffset _ off  -> text (showHex (base+fromIntegral off) "")


ppImm :: (Integral a, Show a) => a -> Doc
ppImm i = text"0x" <> text (showHex i "")

------------------------------------------------------------------------
-- InstructionInstance

type InstructionInstance = InstructionInstanceF (Value, OperandType)

-- | Instruction instance with name and operands.
data InstructionInstanceF a
   = II { iiLockPrefix :: !LockPrefix
          -- | Whether the address size is 16,32, or 64 bits.
          -- Among other things, this is used to determine whether
          -- to use ecx or rcx with the rep prefix.
          --
          -- This is a direct encoding of the @aso@ prefix (address
          -- size override)
        , iiAddrSize :: !SizeConstraint
        , iiOp   :: !String
        , iiArgs :: ![a]
        , iiPrefixes :: !Prefixes
        , iiRequiredPrefix :: Maybe Word8
        , iiOpcode :: [Word8]
        , iiRequiredMod :: Maybe ModConstraint
        , iiRequiredReg :: Maybe Fin8
        , iiRequiredRM :: Maybe Fin8
        }
  deriving (Show, Eq)

instance Functor InstructionInstanceF where
  fmap f ii = ii { iiArgs = fmap f (iiArgs ii) }

-- | Compute the size of an instruction in bytes
instructionSize :: InstructionInstance -> Word8
instructionSize = undefined

padToWidth :: Int -> String -> String
padToWidth n s = if l > 0 then s ++ (replicate (n - l) ' ') else s
  where l = length s

ppPunctuate :: Doc -> [Doc] -> Doc
ppPunctuate p (d1:d2:ds) = d1 <> p <> ppPunctuate p (d2 : ds)
ppPunctuate p (d:[]) = d
ppPunctuate p [] = PP.empty

nonHex1Instrs :: [String]
nonHex1Instrs = ["sar","sal","shr","shl","rcl","rcr","rol","ror"]

ppInstruction :: Word64
                 -- ^ Base address for printing instruction offsets.
                 -- This should be the address of the next instruction.
              -> InstructionInstance
              -> Doc
ppInstruction base i =
  let sLockPrefix = ppLockPrefix (iiLockPrefix i)
      args = map fst (iiArgs i)
      op = iiOp i
  in
   case (op, args)
        -- special casem for one-bit shift instructions
     of (_, [dst, ByteImm 1])
          | op `elem` nonHex1Instrs ->
                                     (text $ padToWidth 6 op)
                                     <+> ppValue base dst
                                     <> comma <> text "1"
        -- objdump prints as nop
        ("xchg", [DWordReg (Reg32 0), DWordReg (Reg32 0)]) -> text "nop"
        _ -> case (args, iiLockPrefix i)
               of ([], NoLockPrefix) -> text op
                  (_, NoLockPrefix) -> (text $ padToWidth 6 op)
                                       <+> (ppPunctuate comma $ ppValue base <$> args)
                  ([], _) -> sLockPrefix <+> text op
                  (_,_) -> sLockPrefix <+> text op
                           <+> (ppPunctuate comma $ ppValue base <$> args)
