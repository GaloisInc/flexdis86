{- |
Module      :  $Header$
Copyright   :  (c) Galois, Inc 2013-2016
Maintainer  :  jhendrix@galois.com

This declares the main datatypes for the instruction set.
-}
{-# LANGUAGE PatternSynonyms #-}
module Flexdis86.InstructionSet
  ( -- * Instruction information
    InstructionInstance
  , InstructionInstanceF(..)
  , ppInstruction
  , Value(..)
  , Displacement(..)
  , AddrRef(..)
  , module Flexdis86.Relocation
  ) where

import           Control.Applicative
import           Data.Int
import           Data.Word
import           Numeric (showHex)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.PrettyPrint.ANSI.Leijen hiding (empty, (<$>))

import           Prelude

import           Flexdis86.Operand
import           Flexdis86.Prefixes
import           Flexdis86.Register
import           Flexdis86.Relocation
import           Flexdis86.Segment
import           Flexdis86.Sizes

padToWidth :: Int -> String -> String
padToWidth n s = if l > 0 then s ++ (replicate (n - l) ' ') else s
  where l = length s

ppPunctuate :: Doc -> [Doc] -> Doc
ppPunctuate p (d1:d2:ds) = d1 <> p <> ppPunctuate p (d2 : ds)
ppPunctuate _ (d:[]) = d
ppPunctuate _ [] = PP.empty

------------------------------------------------------------------------
-- AddrRef

-- | Displacement in an indirect memory reference.
--
-- These can be encoded as either 8 or 32 bits
data Displacement = Disp32 Imm32
                  | Disp8 Int8
                  | NoDisplacement
                  deriving (Show)

prettyDisplacement :: Displacement -> Doc
prettyDisplacement NoDisplacement = text "0"
prettyDisplacement (Disp32 x) = text (show x)
prettyDisplacement (Disp8 x) =
  if x >= 0 then
    text ("0x" ++ showHex x "")
   else
    text ("-0x" ++ showHex (negate (fromIntegral x :: Int16)) "")

-- | Append a displacement to an expression
appendDisplacement :: Displacement -> Doc
appendDisplacement NoDisplacement = text ""
appendDisplacement (Disp32 x)
  | Imm32Concrete 0 <- x = text ""
  | otherwise = text (show x)
appendDisplacement (Disp8 x)
  | x >  0    = text ("+0x" ++ showHex x "")
  | x == 0    = text ""
  | otherwise = text ("-0x" ++ showHex (negate (fromIntegral x :: Int16)) "")

-- | A references to an address in memory.
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
  | IP_Offset_32 !Segment Displacement
  | Offset_32    !Segment !Imm32
    -- ^ A 32bit offset relative to a segment.
  | Offset_64    !Segment !Word64
    -- ^ A 64bit offset relative to a segment.
  | Addr_64      !Segment (Maybe Reg64) (Maybe (Int, Reg64)) Displacement
  | IP_Offset_64 !Segment Displacement
  deriving (Show)

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
      where a = ppAddr base roff off
                                                          -- or rip? this is 32 bits ...
    IP_Offset_32 _seg off     -> brackets $ text "ip" <> appendDisplacement off
    Offset_32 seg off         -> prefix seg off
    Offset_64 seg off         -> prefix seg off
    Addr_64 seg base roff off
        | seg == FS || seg == GS -> text (show seg) <> colon <> a
        | isDef     -> a
        | otherwise -> a
      where a = ppAddr base roff off
            isDef = maybe False (isDefaultSeg64 seg) base

    IP_Offset_64 _seg off -> brackets $ text "rip" <> appendDisplacement off
  where
    prefix seg off = ppShowReg seg <> colon <> text (show off)

    ppAddr :: Show r
           => Maybe r -- Base value
           -> Maybe (Int, r) -- Relative offset
           -> Displacement -- Offset
           -> Doc
    ppAddr base roff off =
      case (base, roff) of
         (Nothing, Nothing)     -> prettyDisplacement off
         (Nothing, Just (n, r)) ->
           brackets (text (show r) <> text "*" <> int n <> appendDisplacement off)
         (Just r, Nothing)      -> brackets $
           text (show r) <> appendDisplacement off
         (Just r, Just (n, r')) ->
           brackets $
             text (show r) <> text "+" <> text (show r') <> text "*" <> int n <> appendDisplacement off

------------------------------------------------------------------------
-- Value

-- | The value of an operand in an instruction instance.
data Value
  = ControlReg ControlReg
  | DebugReg DebugReg
  | MMXReg MMXReg
  | XMMReg XMMReg
  | YMMReg YMMReg
  | SegmentValue Segment
  | X87Register Int
  | FarPointer AddrRef
  | VoidMem AddrRef
  | Mem8  AddrRef
    -- ^ An address to a one byte value.
  | Mem16 AddrRef
    -- ^ An address to a two byte value.
  | Mem32 AddrRef
    -- ^ An address to a four byte value.
  | Mem64 AddrRef
    -- ^ An address to a eight byte value.
  | Mem128 AddrRef
  | Mem256 AddrRef
  | FPMem32 AddrRef
  | FPMem64 AddrRef
  | FPMem80 AddrRef

  | ByteImm  Word8
    -- ^ A 8-bit immediate that should not need to be extended
  | WordImm  Word16
    -- ^ A 16-bit immediate that should not need to be extended
  | DWordImm !Imm32
    -- ^ A 32-bit immmediate that should not need to be extended
  | QWordImm Word64
    -- ^ A 64-bit intermediate that should not need to be extended

  | ByteSignedImm  Int8
    -- ^ A 8-bit constant which may need to be sign extended to the appropriate
    -- size.
  | WordSignedImm  Int16
    -- ^ A 16-bit intermediate that can be sign extended to the appropriate size.
  | DWordSignedImm Int32
    -- ^ A 32-bit intermediate that can be sign extended to the appropriate size.

  | ByteReg  Reg8
  | WordReg  Reg16
  | DWordReg Reg32
  | QWordReg Reg64
  | JumpOffset !JumpSize !JumpOffset
  deriving (Show)

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
    YMMReg       r    -> text (show r)
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
    Mem256       addr -> text "YMMWORD PTR" <+> ppAddrRef addr
    FPMem32      addr -> text "DWORD PTR"   <+> ppAddrRef addr
    FPMem64      addr -> text "QWORD PTR"   <+> ppAddrRef addr
    FPMem80      addr -> text "TBYTE PTR"   <+> ppAddrRef addr
    ByteImm      i  -> text "0x" <> text (showHex i "")
    WordImm      i  -> text "0x" <> text (showHex i "")
    DWordImm     i  -> text (show i)
    QWordImm     i  -> text "0x" <> text (showHex i "")
    ByteSignedImm  i  -> ppImm i
    WordSignedImm  i  -> ppImm i
    DWordSignedImm i  -> ppImm i
    ByteReg      r    -> ppShowReg    r
    WordReg      r    -> ppShowReg    r
    DWordReg     r    -> ppShowReg    r
    QWordReg     r    -> ppShowReg    r
    JumpOffset _ off  -> text (showHex base ("+" ++ show off))


ppImm :: (Integral w, Show w) => w -> Doc
ppImm i | i >= 0 = text"0x" <> text (showHex i "")
          -- Print negation after converting to integer
          -- Recall that  "negate minBound = minBound" with types like Int16, Int32, Int64.
        | otherwise = text"-0x" <> text (showHex (negate (toInteger i)) "")

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

nonHex1Instrs :: [String]
nonHex1Instrs = ["sar","sal","shr","shl","rcl","rcr","rol","ror"]

-- | This pretty prints an instruction using Intel syntax.
ppInstruction :: Word64
                 -- ^ Base address for printing instruction offsets.
                 -- This should be the address of the next instruction.
              -> InstructionInstance
              -> Doc
ppInstruction base i =
  let sLockPrefix = ppLockPrefix (iiLockPrefix i)
      args = fst <$> iiArgs i
      op = iiOp i
  in
   case (op, args) of
        -- special casem for one-bit shift instructions
     (_, [dst, ByteImm 1])
       | op `elem` nonHex1Instrs ->
           text (padToWidth 6 op) <+> ppValue base dst <> comma <> text "1"
     -- objdump prints as nop
     ("xchg", [DWordReg EAX, DWordReg EAX]) -> text "nop"
     _ -> case (args, iiLockPrefix i) of
            ([], NoLockPrefix) -> text op
            (_,  NoLockPrefix) -> text (padToWidth 6 op) <+> ppPunctuate comma (ppValue base <$> args)
            ([], _) -> sLockPrefix <+> text op
            (_,_)   -> sLockPrefix <+> text op <+> ppPunctuate comma (ppValue base <$> args)
