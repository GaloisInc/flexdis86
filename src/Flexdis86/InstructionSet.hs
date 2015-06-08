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
  , Flexdis86.OpTable.SizeConstraint(..)
  , Value(..)
  , ControlReg, controlReg, controlRegNo
  , DebugReg, debugReg, debugRegNo
  , MMXReg, mmx_reg {- deprecated -}, mmxReg, mmxRegNo, mmxRegIdx
  , XMMReg, xmmReg, xmmRegNo, xmmRegIdx
  , LockPrefix(..), ppLockPrefix
  , Segment, es, cs, ss, ds, fs, gs, segmentRegisterByIndex, segmentRegNo
  , AddrRef(..)
  , Word8
  , Word16
  , Word32
  , Word64
  , Reg8, low_reg8, high_reg8, al, bl, cl, dl, ah, bh, ch, dh, is_low_reg, is_high_reg
  , Reg16, reg16, ax, bx, cx, dx, reg16_reg
  , Reg32, reg32, eax, ebx, ecx, edx, esp, ebp, esi, edi, reg32_reg
  , Reg64, reg64, reg64No, reg64Idx, rax, rbx, rcx, rdx, rsp, rbp, rsi, rdi
  , Int64
  ) where

import Control.Applicative
import Control.Exception
import Data.Int
import Data.Word
import qualified Data.Vector as V
import Numeric (showHex)
import Text.PrettyPrint.ANSI.Leijen hiding (empty, (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Flexdis86.OpTable (SizeConstraint(..))

showReg :: Show a => String -> a -> String
showReg p v = "%" ++ p ++ show v

-- | There are 16 control registers CR0 through CR15.
newtype ControlReg = CR Word8
  deriving Eq

instance Show ControlReg where
  show (CR w) = "cr" ++ show w

controlReg :: Word8 -> ControlReg
controlReg w = assert (w < 16) $ CR w

controlRegNo :: ControlReg -> Word8
controlRegNo (CR w) = w

-- | There are 8 32-bit debug registers in ia32, and 16 64-bit
-- debug registers in ia64.
newtype DebugReg = DR Word8
  deriving (Eq)

instance Show DebugReg where
  show (DR w) = "dr" ++ show w

debugReg :: Word8 -> DebugReg
debugReg w = assert (w < 16) $ DR w

debugRegNo :: DebugReg -> Word8
debugRegNo (DR w) = w

-- | There are 8 64-bit MMX registers
newtype MMXReg = MMXR Word8
  deriving (Eq)

instance Show MMXReg where
  show (MMXR w) = showReg "mm" w

{-# DEPRECATED mmx_reg "Use mmxReg instead!" #-}
mmx_reg :: Word8 -> MMXReg
mmx_reg w = assert (w < 8) $ MMXR w

mmxReg :: Word8 -> MMXReg
mmxReg w = assert (w < 8) $ MMXR w

mmxRegNo :: MMXReg -> Word8
mmxRegNo (MMXR w) = w

mmxRegIdx :: MMXReg -> Int
mmxRegIdx = fromIntegral . mmxRegNo

-- | There are 16 128-bit XMM registers
newtype XMMReg = XMMR Word8
  deriving (Eq)

instance Show XMMReg where
  show (XMMR w) = showReg "xmm" w

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

segmentRegNo :: Segment -> Word8
segmentRegNo (Segment r) = r

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
         of Just r | isDefaultSeg64 seg r -> id
                   | seg == fs -> ((text (show seg) <> colon) <+>)
                   | seg == gs -> ((text (show seg) <> colon) <+>)
                   | otherwise -> id -- ((text (show seg) <> colon) <+>)
            _ -> id) (ppAddr seg base roff off)
    IP_Offset_64 seg off      -> brackets $ text "rip+"<> pp0xHex off
  where
    prefix seg off = ppShowReg seg <> colon <> text (show off)
    addOrSub n = text $ if n >= 0 then "+" else "-"
    ppAddr seg base roff off =
      case (base, roff, off) of
         (Nothing, Nothing, 0)      -> PP.empty -- can this happen?
         (Nothing, Just (n, r), 0)  -> brackets (ppShowReg r <> text "*" <> int n)
         (Just r, Nothing, 0)      -> brackets (ppShowReg r)
         (Just r, Just (n, r'), 0)  -> brackets (ppShowReg r <> text "+" <> ppShowReg r' <> text "*" <> int n)
         (Nothing, Nothing, n')     -> brackets $ ppSigned0xHex n'
         (Just r, Nothing, n')      -> brackets (ppShowReg r <> addOrSub n' <> pp0xHex (abs n'))
         (Nothing, Just (n, r), n') -> brackets (ppShowReg r <> text "*" <> int n <> addOrSub n' <> pp0xHex (abs n'))
         (Just r, Just (n, r'), n') -> brackets (ppShowReg r <> text "+" <> ppShowReg r' <> text "*" <> int n <> addOrSub n' <> pp0xHex (abs n'))

isDefaultSeg32 :: Segment -> Reg32 -> Bool
isDefaultSeg32 seg reg = isDefaultSeg64 seg $ reg32_reg reg

isDefaultSeg64 :: Segment -> Reg64 -> Bool
isDefaultSeg64 (Segment 2) (Reg64  4) = True
isDefaultSeg64 (Segment 2) (Reg64  5) = True
isDefaultSeg64 (Segment 3) (Reg64  0) = True
isDefaultSeg64 (Segment 3) (Reg64  1) = True
isDefaultSeg64 (Segment 3) (Reg64  2) = True
isDefaultSeg64 (Segment 3) (Reg64  3) = True
isDefaultSeg64 (Segment 3) (Reg64  6) = True
isDefaultSeg64 (Segment 3) (Reg64  7) = True
isDefaultSeg64 (Segment 3) (Reg64  8) = True
isDefaultSeg64 (Segment 3) (Reg64  9) = True
isDefaultSeg64 (Segment 3) (Reg64 10) = True
isDefaultSeg64 (Segment 3) (Reg64 11) = True
isDefaultSeg64 (Segment 3) (Reg64 12) = True
isDefaultSeg64 (Segment 3) (Reg64 13) = True
isDefaultSeg64 (Segment 3) (Reg64 14) = True
isDefaultSeg64 (Segment 3) (Reg64 15) = True
isDefaultSeg64 _ _ = False


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
    X87Register  n    -> text "st" <> parens (int n)
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

data LockPrefix
   = NoLockPrefix
   | RepPrefix
   | RepZPrefix
  deriving (Show, Eq)

ppLockPrefix :: LockPrefix -> Doc
ppLockPrefix NoLockPrefix = PP.empty
ppLockPrefix RepPrefix  = text "rep"
ppLockPrefix RepZPrefix = text "repz"

-- | Instruction instance with name and operands.
data InstructionInstance
   = II { iiLockPrefix :: !LockPrefix
          -- | Whether the address size is 16,32, or 64 bits.
          -- Among other things, this is used to determine whether
          -- to use ecx or rcx with the rep prefix.
        , iiAddrSize :: !SizeConstraint
        , iiOp   :: !String
        , iiArgs :: ![Value]
        }
  deriving (Show, Eq)

padToWidth :: Int -> String -> String
padToWidth n s = if l > 0 then s ++ (replicate (n - l) ' ') else s
  where l = length s

ppPunctuate :: Doc -> [Doc] -> Doc
ppPunctuate p (d1:d2:ds) = d1 <> p <> ppPunctuate p (d2 : ds)
ppPunctuate p (d:[]) = d
ppPunctuate p [] = PP.empty


ppInstruction :: Word64
                 -- ^ Base address for printing instruction offsets.
                 -- This should be the address of the next instruction.
              -> InstructionInstance
              -> Doc
ppInstruction base i = 
  let sLockPrefix = ppLockPrefix (iiLockPrefix i)
  in (if iiLockPrefix i == NoLockPrefix then id else (sLockPrefix <+>))
    (text $ padToWidth 6 $ iiOp i)
     <+> (ppPunctuate comma $ ppValue base <$> iiArgs i)
