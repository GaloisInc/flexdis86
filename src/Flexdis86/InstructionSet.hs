{- |
Module      :  $Header$
Copyright   :  (c) Galois, Inc 2013-2016
Maintainer  :  jhendrix@galois.com

This declares the main datatypes for the instruction set.
-}

{-# LANGUAGE OverloadedStrings #-}

module Flexdis86.InstructionSet
  ( -- * Instruction information
    InstructionInstance
  , InstructionInstanceF(..)
  , ppInstruction
  , ppInstructionWith
  , Value(..)
  , ppValue
  , Displacement(..)
  , AddrRef(..)
  , module Flexdis86.Relocation
  ) where

import           Control.Applicative
import qualified Data.ByteString.Char8 as BSC
import           Data.Int
import           Data.String (fromString)
import           Data.Word
import           Numeric (showHex)
import qualified Prettyprinter as PP

import           Prelude

import           Flexdis86.Operand
import           Flexdis86.Prefixes
import           Flexdis86.Register
import           Flexdis86.Relocation
import           Flexdis86.Segment
import           Flexdis86.Sizes

padToWidth :: Int -> String -> PP.Doc a
padToWidth n s = fromString $ if l > 0 then s ++ replicate (n - l) ' ' else s
  where l = length s

ppPunctuate :: PP.Doc a -> [PP.Doc a] -> PP.Doc a
ppPunctuate p = PP.hsep . PP.punctuate p

------------------------------------------------------------------------
-- AddrRef

-- | Displacement in an indirect memory reference.
--
-- These can be encoded as either 8 or 32 bits
data Displacement = Disp32 Imm32
                  | Disp8 Int8
                  | NoDisplacement
                  deriving (Show, Eq, Ord)

prettyDisplacement :: Displacement -> PP.Doc a
prettyDisplacement NoDisplacement = "0"
prettyDisplacement (Disp32 x) = fromString (show x)
prettyDisplacement (Disp8 x) = fromString $
  if x >= 0
    then "0x" ++ showHex x ""
    else "-0x" ++ showHex (negate (fromIntegral x :: Int16)) ""

-- | Append a displacement to an expression
appendDisplacement :: Displacement -> PP.Doc a
appendDisplacement NoDisplacement = ""
appendDisplacement (Disp32 x)
  | Imm32Concrete 0 <- x = ""
  | otherwise = fromString (show x)
appendDisplacement (Disp8 x)
  | x >  0    = fromString ("+0x" ++ showHex x "")
  | x == 0    = ""
  | otherwise = fromString ("-0x" ++ showHex (negate (fromIntegral x :: Int16)) "")

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
  deriving (Show, Eq, Ord)

ppAddrRef :: AddrRef -> PP.Doc a
ppAddrRef addr =
  case addr of
    Addr_32 seg base roff off ->
       case base of
         Just r | isDefaultSeg32 seg r -> a
                | seg == FS -> PP.pretty seg <> ":" PP.<+> a
                | seg == GS -> PP.pretty seg <> ":" PP.<+> a
                | otherwise -> a -- (((show seg) <> colon) PP.<+>)
         _ -> a
      where a = ppAddr base roff off
                                                          -- or rip? this is 32 bits ...
    IP_Offset_32 _seg off     -> PP.brackets $ "ip" <> appendDisplacement off
    Offset_32 seg off         -> prefix seg off
    Offset_64 seg off         -> prefix seg off
    Addr_64 seg base roff off
        | seg == FS || seg == GS -> PP.pretty seg <> ":" <> a
        | isDef     -> a
        | otherwise -> a
      where a = ppAddr base roff off
            isDef = maybe False (isDefaultSeg64 seg) base

    IP_Offset_64 _seg off -> PP.brackets $ "rip" <> appendDisplacement off
  where
    prefix seg off = PP.pretty seg <> ":" <> fromString (show off)

    ppAddr :: PP.Pretty r
           => Maybe r -- Base value
           -> Maybe (Int, r) -- Relative offset
           -> Displacement -- Offset
           -> PP.Doc a
    ppAddr base roff off =
      case (base, roff) of
         (Nothing, Nothing)     -> prettyDisplacement off
         (Nothing, Just (n, r)) ->
           PP.brackets (PP.pretty r <> "*" <> PP.pretty n <> appendDisplacement off)
         (Just r, Nothing)      -> PP.brackets $
           PP.pretty r <> appendDisplacement off
         (Just r, Just (n, r')) ->
           PP.brackets $
             PP.pretty r <> "+" <> PP.pretty r' <> "*" <> PP.pretty n <> appendDisplacement off

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
  | QWordImm !UImm64
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
  deriving (Show, Eq, Ord)

ppValue :: Value -> PP.Doc a
ppValue v =
  case v of
    ControlReg   r    -> PP.pretty r
    DebugReg     r    -> PP.pretty r
    MMXReg       r    -> PP.pretty r
    XMMReg       r    -> PP.pretty r
    YMMReg       r    -> PP.pretty r
    X87Register  n    -> "st" <> if n == 0 then "" else PP.parens (PP.pretty n)
    SegmentValue r    -> PP.pretty r
    -- do the "*" belong here or in ppAddrRef?
    FarPointer   addr -> "??FAR PTR??" PP.<+> ppAddrRef    addr
    VoidMem      addr -> ppAddrRef addr
    Mem8         addr -> "BYTE PTR"    PP.<+> ppAddrRef addr
    Mem16        addr -> "WORD PTR"    PP.<+> ppAddrRef addr
    Mem32        addr -> "DWORD PTR"   PP.<+> ppAddrRef addr
    Mem64        addr -> "QWORD PTR"   PP.<+> ppAddrRef addr
    Mem128       addr -> "XMMWORD PTR" PP.<+> ppAddrRef addr
    Mem256       addr -> "YMMWORD PTR" PP.<+> ppAddrRef addr
    FPMem32      addr -> "DWORD PTR"   PP.<+> ppAddrRef addr
    FPMem64      addr -> "QWORD PTR"   PP.<+> ppAddrRef addr
    FPMem80      addr -> "TBYTE PTR"   PP.<+> ppAddrRef addr
    ByteImm      i  -> "0x" <> fromString (showHex i "")
    WordImm      i  -> "0x" <> fromString (showHex i "")
    DWordImm     i  -> PP.pretty i
    QWordImm     i  -> PP.pretty i
    ByteSignedImm  i  -> ppImm i
    WordSignedImm  i  -> ppImm i
    DWordSignedImm i  -> ppImm i
    ByteReg  r -> PP.pretty r
    WordReg  r -> PP.pretty r
    DWordReg r -> PP.pretty r
    QWordReg r -> PP.pretty r
    JumpOffset _ off  -> "pc+" <> PP.pretty off


ppImm :: (Integral w, Show w) => w -> PP.Doc a
ppImm i | i >= 0 = "0x" <> fromString (showHex i "")
          -- PrPP.pretty negation after converting to integer
          -- Recall that  "negate minBound = minBound" with types like Int16, Int32, Int64.
        | otherwise = "-0x" <> fromString (showHex (negate (toInteger i)) "")

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
        , iiOp   :: !BSC.ByteString
        , iiArgs :: ![a]
        , iiPrefixes :: !Prefixes
        , iiRequiredPrefix :: Maybe Word8
          -- | List of opcodes, which should always be nonempty.
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

-- | This pretty prints an instruction using close to Intel syntax.
-- Jump offsets refer to PC rather than the relative address.
ppInstruction :: InstructionInstance -> PP.Doc a
ppInstruction i =
  let sLockPrefix = ppLockPrefix (iiLockPrefix i)
      args = fst <$> iiArgs i
      op = BSC.unpack (iiOp i)
  in
   case (op, args) of
        -- special casem for one-bit shift instructions
     (_, [dst, ByteImm 1])
       | op `elem` nonHex1Instrs ->
           padToWidth 6 op PP.<+> ppValue dst <> ",1"
     -- objdump prints `xchg (e)ax,(e)ax` as nop
     ("xchg", [WordReg   AX, WordReg   AX]) -> "nop"
     ("xchg", [DWordReg EAX, DWordReg EAX]) -> "nop"
     _ -> case (args, iiLockPrefix i) of
            ([], NoLockPrefix) -> fromString op
            (_,  NoLockPrefix) -> padToWidth 6 op PP.<+> ppPunctuate "," (ppValue <$> args)
            ([], _) -> sLockPrefix PP.<+> fromString op
            (_,_)   -> sLockPrefix PP.<+> fromString op PP.<+> ppPunctuate "," (ppValue <$> args)

ppInstructionWith :: (i -> PP.Doc a)
                  -> InstructionInstanceF i
                  -> PP.Doc a
ppInstructionWith ppv i =
  -- FIXME Too much copy-and-paste, but not clear how to abstract
  -- given the special cases
  let sLockPrefix = ppLockPrefix (iiLockPrefix i)
      args = iiArgs i
      op = BSC.unpack (iiOp i)
  in
  case (args, iiLockPrefix i) of
    ([], NoLockPrefix) -> fromString op
    (_,  NoLockPrefix) -> padToWidth 6 op PP.<+> ppPunctuate "," (ppv <$> args)
    ([], _) -> sLockPrefix PP.<+> fromString op
    (_,_)   -> sLockPrefix PP.<+> fromString op PP.<+> ppPunctuate "," (ppv <$> args)
