{- |
Module      :  $Header$
Description :  Declares datatypes for the instruction set.
Copyright   :  (c) Galois, Inc 2013
Maintainer  :  jhendrix@galois.com

This declares the main datatypes for the instruction set.
-}
module Flexdis86.InstructionSet
  ( InstructionInstance(..)
  , ppInstruction
  , instructionSize
  , Flexdis86.OpTable.SizeConstraint(..)
  , Value(..)
  , ControlReg, controlReg, controlRegNo
  , DebugReg, debugReg, debugRegNo
  , MMXReg, mmxReg, mmxRegNo, mmxRegIdx
  , XMMReg, xmmReg, xmmRegNo, xmmRegIdx
  , LockPrefix(..), ppLockPrefix
  , Segment, es, cs, ss, ds, fs, gs, segmentRegisterByIndex, segmentRegNo
  , AddrRef(..)
  , Word8
  , Word16
  , Word32
  , Word64
  , Int64
  ) where

import Control.Applicative
import Control.Exception
import Data.Int
import Data.Word
import Data.Bits
import qualified Data.Vector as V
import Numeric (showHex)
import Text.PrettyPrint.ANSI.Leijen hiding (empty, (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Flexdis86.OpTable (SizeConstraint(..), Def)
import Flexdis86.Prefixes
import Flexdis86.Register
import Flexdis86.Segment



------------------------------------------------------------------------
-- AddrRef

data AddrRef
    -- | @Addr_32 s b i o@ denotes a 32-bit IP address that will
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
      (case base 
         of Just r | isDefaultSeg32 seg r -> id
                   | seg == fs -> ((text (show seg) <> colon) <+>) 
                   | seg == gs -> ((text (show seg) <> colon) <+>) 
                   | otherwise -> id -- ((text (show seg) <> colon) <+>)
            _ -> id) (ppAddr seg base roff off)
                                                          -- or rip? this is 32 bits ...
    IP_Offset_32 seg off      -> brackets $ text "ip+" <> pp0xHex off
    Offset_32 seg off         -> prefix seg off
    Offset_64 seg off         -> prefix seg off
    Addr_64 seg base roff off -> 
      (case base 
         of Just r  | seg == fs -> ((text (show seg) <> colon) <>)
                    | seg == gs -> ((text (show seg) <> colon) <>)
                    | isDefaultSeg64 seg r -> id
                    | otherwise -> id -- ((text (show seg) <> colon) <+>)
            Nothing | seg == fs -> ((text (show seg) <> colon) <>)
                    | seg == gs -> ((text (show seg) <> colon) <>)
	            | otherwise -> id) (ppAddr seg base roff off)
    IP_Offset_64 seg off      -> brackets $ text "rip+"<> pp0xHex off
  where
    prefix seg off = ppShowReg seg <> colon <> text (show off)
    addOrSub n = text $ if n >= 0 then "+" else "-"
    ppAddr seg base roff off =
      case (base, roff, off) of
         (Nothing, Nothing, 0)      -> text "0x0" -- happens with fs and gs segments
         (Nothing, Just (n, r), 0)  -> brackets (ppShowReg r <> text "*" <> int n)
         (Just r, Nothing, 0)      -> brackets (ppShowReg r)
         (Just r, Just (n, r'), 0)  -> brackets (ppShowReg r <> text "+" <> ppShowReg r' <> text "*" <> int n)
         (Nothing, Nothing, n')     -> pp0xHex n'
         (Just r, Nothing, n')      -> brackets (ppShowReg r <> addOrSub n' <>  pp0xHex (abs n'))
         (Nothing, Just (n, r), n') -> brackets (ppShowReg r <> text "*" <> int n <> addOrSub n' <> pp0xHex (abs n'))
         (Just r, Just (n, r'), n') -> brackets (ppShowReg r <> text "+" <> ppShowReg r' <> text "*" <> int n <> addOrSub (fromIntegral n') <> pp0xHex (abs n'))



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
  | JumpOffset Int64
  deriving (Show, Eq)

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
    JumpOffset   off  -> text (showHex (base+fromIntegral off) "")


ppImm :: (Integral a, Show a) => a -> Doc
ppImm i = text"0x" <> text (showHex i "")

------------------------------------------------------------------------
-- InstructionInstance


-- | Instruction instance with name and operands.
data InstructionInstance
   = II { iiLockPrefix :: !LockPrefix
          -- | Whether the address size is 16,32, or 64 bits.
          -- Among other things, this is used to determine whether
          -- to use ecx or rcx with the rep prefix.
          --
          -- This is a direct encoding of the @aso@ prefix (address
          -- size override)
        , iiAddrSize :: !SizeConstraint
        , iiOp   :: !String
        , iiArgs :: ![Value]
        , iiPrefixes :: !Prefixes
        , iiRequiredPrefix :: Maybe Word8
        , iiOpcode :: [Word8]
        , iiHasModRM :: Bool
          -- ^ True if the instruction requires a ModRM byte.  We'll
          -- compute that based on the operands later.
        }
  deriving (Show, Eq)


instructionSize :: InstructionInstance -> Int
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
      args = iiArgs i
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
