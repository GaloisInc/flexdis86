{- |
Module      :  $Header$
Copyright   :  (c) Galois, Inc
Maintainer  :  jhendrix@galois.com

This defines a disassembler based on optable definitions.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}
module Flexdis86.Disassembler
  ( mkX64Disassembler
  , NextOpcodeTable
  , nextOpcodeSize
  , disassembleInstruction
  , DisassembledAddr(..)
  , tryDisassemble
  , disassembleBuffer
  ) where

import           Control.Applicative
import qualified Control.DeepSeq as DS
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Binary.Get (Decoder(..), runGetIncremental)
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Either (isLeft)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as F
import           GHC.Generics
import           Data.Int
import           Data.List (subsequences, partition)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Word
import           GHC.Stack

import           Prelude

import           Flexdis86.ByteReader
import           Flexdis86.InstructionSet
import           Flexdis86.OpTable
import           Flexdis86.Operand
import           Flexdis86.Prefixes
import           Flexdis86.Register
import           Flexdis86.Segment
import           Flexdis86.Sizes

{-
Note [x86_64 disassembly]
~~~~~~~~~~~~~~~~~~~~~~~~~
Writing an x86_64 disassembler is surprisingly nuanced, and it has taken us
several iterations to get it right. This Note exists to document some of the
less obvious choices that we have made.

The disassembler turns an instruction stream (i.e., a sequence of bytes) into
an InstructionInstance. The structure of an instruction stream is roughly this:

  -----------------------------------------------
  | Prefix bytes | Opcode bytes | Operand bytes |
  -----------------------------------------------

The data/optable.xml file, which contains every x86_64 instruction that
flexdis86 supports, generally represents prefix bytes with the <pfx> element,
opcode bytes with the <opc> element, and operand bytes with the <opr> byte.
Note that there are a handful of exceptions to this rule—for instance, VEX
prefixes are indicated with /vex, which appear in <opr> rather than <pfx>.

Note that while the opcodes will always be present in the instruction stream,
not every instruction has prefixes or operands. Moreover, we cannot know
whether an instruction expects certain prefixes or operands without first
disassembling its opcodes. Once we have disassembled the opcodes, we know what
instruction we have, so we can simply choose whether to disassemble operands
afterwards. However, the prefixes pose more of a challenge, since we have to
parse them /before/ we know what instruction we have.

One approach to handling prefixes is to analyze all instructions ahead of time,
compute all possible permutations of prefix bytes and opcode bytes that each
instruction can accept, and encode all of these different paths into a lookup
table. This way, there is never any doubt about whether the set of prefixes
encountered thus far are legal or not, as the only legal sets of prefixes are
those with corresponding paths in the lookup table. That is, we have:

          Lookup table
    ------------^-------------
   /                          \
  /                            \
  -----------------------------------------------
  | Prefix bytes | Opcode bytes | Operand bytes |
  -----------------------------------------------

The downside to the all-possible-permutations approach is that it is fairly
expensive. The lookup table has about 5.8 million nodes when it is finally
constructed. Not only does this take an appreciable amount of time to build at
runtime, but because this table must be kept alive by the garbage collector, it
also uses several hundreds of megabytes of memory. We ought to be able to do
better!

An alternative approach, which flexdis86 currently uses, is to build a more
modest lookup table that only encodes the opcode bytes of each instruction
without any of the prefix bytes. (Actually, this isn't /quite/ accurate, but
suspend your disbelief for the time being. We'll provide a more complete
explanation later.) Instead, we begin disassembly by parsing as many bytes as
possible that are known to be prefix bytes. These include:

* Simple "legacy" prefixes (see `simplePrefixBytes`)
* Segment prefixes (see `segPrefixBytes`)
* REX prefixes (see `rexPrefixBytes`)
* VEX prefixes (see `mkVexPrefixes`)

Once we encounter the first byte that is not a known prefix byte, we begin
consulting the lookup table to disassemble all of the opcode bytes. This is
often enough to uniquely determine what kind of instruction we have, but there
are also some opcodes that are used across multiple instructions. In case of
the latter, we can compare the set of disassembled prefixes to what each
instruction candidate permits, filtering out instructions that do not allow
one of the disassembled prefixes. If everything goes according to plan, there
should be exactly one instruction among the candidates that permits the set of
disassembled prefixes and opcodes.

This plan, while reasonable-looking at a first glance, is unfortunately not
enough to handle all of the curveballs that x86_64 throws. The plan makes the
assumption that the set of possible prefix bytes is disjoint from the set of
bytes that appear as an instruction's first opcode byte, but this is not the
case. Here are some examples of instructions that violate this assumption:

* The lds and les instructions' opcodes begin with 0xc5 and 0xc4, respectively,
  which clash with the two-byte and three-byte VEX prefixes, respectively.
* The inc instruction can have opcodes beginning with 0x40 through 0x47, and
  the dec instruction can have opcodes beginning with 0x48 through 0x4f. All of
  these clash with possible REX prefixes.
* Several vector instructions, such as vpcmpgtd, vpslld, vpsllq, and
  vpclmulqdq, also have opcodes that begin with a byte that is also used as
  a prefix of some sort.
* The endbr32, endbr64, and pause instructions all have opcodes beginning with
  0xf3, which conflict with the REP prefix.
* The xchg instruction's opcode can begin with 0x66, which conflicts with the
  OSO prefix.

As a result, disassembling prefix bytes eagerly can cause us to prematurely
read off bytes that should be opcode bytes, not prefix bytes. Sigh.

One possible mitigation is to perform backtracking during disassembly so that
we handle all instructions that could introduce ambiguity during parsing. This
would add an unfortunate amount of complexity, however, so we made a concerted
effort to avoid this if possible. Luckily, it turns out that we /can/ avoid
this with some ingenuity.

First, let's talk about the lds, les, inc, and dec instructions. While
potentially problematic, they are also instructions that aren't valid in 64-bit
mode. Since our x86_64 disassembler only cares about 64-bit mode, we can simply
pretend like these instructions don't exist. (If we ever decide to expand the
capabilities of the disassembler to include these instructions, we will have to
revisit this choice.)

Next up are the ambiguous vector instructions. One thing that all of these
instructions have in common is that they all have VEX prefixes. VEX prefixes
are rather unique in that:

* If an instruction accepts VEX prefixes, then at least one valid set of VEX
  prefix bytes must be present. It is not allowed to omit the VEX prefix bytes
  entirely.
* The VEX prefixes always appear at the very end of the prefix bytes, just
  before the instruction's opcode bytes.

A useful consequence of these two properties is that VEX prefix bytes are a
useful tool for disambiguating instructions with similar opcodes. And in fact,
if you factor in VEX prefix bytes, then the vector instructions listed above
aren't ambiguous at all, since the other instructions that share their opcodes
do not accept VEX prefixes. As a result, we have decided to carve out a very
special case in the lookup table and include VEX prefix bytes (but not any
other forms of prefix bytes) in the paths. That is, we have:

                                           Lookup table
                                   --------------^---------------
                                  /                              \
                                 /                                \
  ----------------------------------------------------------------------------------
  | Prefix bytes (excluding VEX) | VEX prefix bytes | Opcode bytes | Operand bytes |
  ----------------------------------------------------------------------------------

One downside to this special case is that we must increase the size of the
lookup table slightly to accommodate VEX prefix bytes. Without VEX prefix
bytes, the size of the table would be about 1,400 nodes, but with VEX prefix
bytes, the size is about 13,000 nodes. That's still several orders of magnitude
smaller than 5.8 million nodes, however, so we consider this an acceptable
price to pay.

The remaining challenges are the following instructions, which are all
particular encodings of the nop instruction:

* endbr32 and endbr64
* pause
* xchg

First, let's discuss endbr32 and endbr64. Although the first byte in their
opcodes (0xf3) is ambiguous, the remaining bytes are unique. As a result, we
encode these instructions into the lookup table using their second opcode bytes
and onward, and after we have identified the instruction as endbr{32,64} during
disassembly, we check that the 0xf3 byte has been parsed as a prefix, ruling
them out if this is not the case. After we have determined that they are in
fact endbr{32,64}, we add the 0xf3 part back to their opcodes in the Def.

Next up is the pause instruction. The opcodes for pause and nop are 0xf3 0x90
and 0x90, respectively, so this time we cannot disambiguate the two by using a
suffix of their opcode bytes. Moreover, neither pause nor nop accept any
prefixes, so we can't disambiguate them that way. With all other options
exhausted, we are forced to perform some invasive surgery on data/optable.xml.
Namely, we:

* Remove pause's entry from data/optable.xml and change nop's entry to indicate
  that nop accepts a REP prefix byte, i.e., 0xf3. (This is a lie, but we will
  make up for our fibbing later.)
* After we have identified an instruction as nop during disassembly, we check
  to see if we have parsed a REP prefix. If so, we know that we have actually
  encountered a pause instruction, so we fixup the instruction mnemonic to
  refer to pause instead of nop after the fact.

Finally, xchg poses a similar challenge as pause. The `xchg ax,ax` instruction
is encoded as 0x66 0x90, where 0x66 is an operand-size prefix and 0x90 is the
opcode, the latter of which is shared with nop. Moreover, the ISA manual entry
for xchg (https://www.felixcloutier.com/x86/xchg) specifically calls out
`xchg ax,ax` as an alias for nop. As a result, we apply a similar hack for
xchg: we remove its entry from data/opcode.xml, indicate that nop can accept an
operand-size prefix (another lie) and fixup the nop instruction to xchg during
disassembly after checking its prefixes. Gross, but effective.
-}

------------------------------------------------------------------------
-- REX

rex_instr_pfx :: Word8
rex_instr_pfx = 0x40

rex_w_bit, rex_r_bit, rex_x_bit, rex_b_bit :: Word8
rex_w_bit = 0x08
rex_r_bit = 0x04
rex_x_bit = 0x02
rex_b_bit = 0x01

no_rex :: REX
no_rex = REX 0

-- | Extension of ModR/M reg field.
rex_r :: REX -> Word8
rex_r r = (unREX r `shiftL` 1) .&. 0x8

-- | Extension of SIB index field.
rex_x :: REX -> Word8
rex_x r = (unREX r `shiftL` 2) .&. 0x8

-- | Extension of ModR/M r/m field, SIB base field, or Opcode reg field.
rex_b :: REX -> Word8
rex_b r = (unREX r `shiftL` 3) .&. 0x8

reg8 :: REX -> Word8 -> Reg8
reg8 rex w | rex == no_rex = if w < 4 then LowReg8 w else HighReg8 (w-4)
           | otherwise     = LowReg8 w

------------------------------------------------------------------------
-- ModRM

-- | ModR/M value
newtype ModRM = ModRM { unModRM :: Word8 }

modRM_mod :: ModRM -> Word8
modRM_mod m = (unModRM m `shiftR` 6) .&. 3

modRM_reg :: ModRM -> Word8
modRM_reg m = (unModRM m `shiftR` 3) .&. 7

modRM_rm :: ModRM -> Word8
modRM_rm m = unModRM m .&. 7

------------------------------------------------------------------------
-- SIB

-- | SIB Byte value
newtype SIB = SIB { unSIB :: Word8 }

sib_scale :: SIB -> Word8
sib_scale s = unSIB s `shiftR` 6

sib_index :: SIB -> Word8
sib_index s = (unSIB s `shiftR` 3) .&. 0x7

sib_base :: SIB -> Word8
sib_base s = unSIB s .&. 0x7

------------------------------------------------------------------------
-- Misc

memRef :: Bool -- ^ ASO
       -> Segment -- ^ Segment to load
       -> Maybe Word8
       -> Maybe (Int,Word8)
       -> Displacement
       -> AddrRef
memRef True  s b si o = Addr_32 s (Reg32 <$> b) (over _2 Reg32 <$> si) o
memRef False s b si o = Addr_64 s (Reg64 <$> b) (over _2 Reg64 <$> si) o

rsp_idx :: Word8
rsp_idx = 4

rbp_idx :: Word8
rbp_idx = 5

sib_si :: (Word8 -> Word8) -> SIB -> Maybe (Int,Word8)
sib_si index_reg sib | idx == rsp_idx = Nothing
                     | otherwise      = Just (2^sib_scale sib, idx)
  where idx = index_reg $ sib_index sib


data RegTable a
   = RegTable !(V.Vector a)
   | RegUnchecked !a
  deriving (Generic, Show)

instance DS.NFData a => DS.NFData (RegTable a)

type RMTable = RegTable [(Maybe VEX, Def)]

data ModTable
     -- | @ModTable memTable regTable@
   = ModTable !RMTable !RMTable
   | ModUnchecked !RMTable
  deriving (Generic, Show)

instance DS.NFData ModTable

------------------------------------------------------------------------
-- OpcodeTable/NextOpcodeTable mutually recursive definitions

-- | A table used to look up instructions based on their VEX prefix and opcode
-- bytes during disassembly. See @Note [x86_64 disassembly]@ for more on how
-- this table works.
data OpcodeTable
   = -- | There are still more opcode bytes to parse.
     OpcodeTable !NextOpcodeTable
     -- | All of the opcode bytes have been parsed, resulting in a list of
     -- possible instruction candidates.
   | OpcodeTableEntry
       ![(Maybe VEX, Def)] -- Defs expecting a ModR/M byte
       ![(Maybe VEX, Def)] -- Defs not expecting a ModR/M byte
  deriving (Generic)

instance DS.NFData OpcodeTable

-- | A NextOpcodeTable describes a table of parsers to read based on the bytes.
type NextOpcodeTable = V.Vector OpcodeTable

------------------------------------------------------------------------
-- OpcodeTable/NextOpcodeTable instances

deriving instance Show OpcodeTable

type ParserGen = Except String

runParserGen :: ParserGen a -> Either String a
runParserGen p = runExcept p

type DefTableFn m t = [(Maybe VEX, Def)] -> m t
-- ^ Given a list of pairs of VEX prefixes and definitions, parse a table of
-- possible instructions in the supplied 'Monad'.

prefixOperandSizeConstraint :: Prefixes -> Def -> OperandSizeConstraint
prefixOperandSizeConstraint pfx d
  -- if the instruction defaults to 64 bit or REX.W is set, then we get 64 bits
  | Just Default64 <- d^.defMode,
    pfx^.prOSO == False = OpSize64
  | (getREX pfx)^.rexW  = OpSize64
  | pfx^.prOSO          = OpSize16
  | otherwise           = OpSize32

-- | Returns true if this definition supports the given operand size constaint.
matchRequiredOpSize :: Prefixes -> Def -> Bool
matchRequiredOpSize pfx d =
  case d^.reqOpSize of
    Just sc -> sc == prefixOperandSizeConstraint pfx d
    Nothing -> True

-- | Returns true if this instruction depends on modrm byte.
expectsModRM :: Def -> Bool
expectsModRM d
  =  isJust (d^.requiredMod)
  || isJust (d^.requiredReg)
  || isJust (d^.requiredRM)
  || isJust (d^.x87ModRM)
  || any modRMOperand (d^.defOperands)

-- | Returns true if operand has a modRMOperand.
modRMOperand :: OperandType -> Bool
modRMOperand nm =
  case nm of
    AbsoluteAddr -> False
    OpType sc _ ->
      case sc of
        ModRM_rm        -> True
        ModRM_rm_mod3   -> True
        ModRM_reg       -> True
        Opcode_reg _    -> False
        Reg_fixed _     -> False
        VVVV            -> False
        ImmediateSource -> False
        OffsetSource    -> False
        JumpImmediate   -> False
    RG_C   -> True
    RG_dbg -> True
    RG_S   -> True
    RG_ST _ -> False -- FIXME(?)
    RG_MMX_reg -> True
    RG_XMM_reg {} -> True
    RG_XMM_rm {} -> True
    RM_XMM {} -> True
    SEG _ -> False
    M_FP  -> True
    M     -> True
    M_U _ -> True
    M_X _ -> True
    M_FloatingPoint _ -> True
    MXRX _ _ -> True
    RM_MMX    -> True
    RG_MMX_rm -> True
    M_Implicit{} -> False
    IM_1  -> False
    IM_SB -> False
    IM_SZ -> False
    VVVV_XMM {} -> False

-- | Split entries based on first identifier in list of bytes.
partitionBy :: [([Word8], a)] -> V.Vector [([Word8], a)]
partitionBy l = V.create $ do
  mv <- VM.replicate 256 []
  let  go ([], _d) = pure ()
       go (w:wl,d) = do
         el <- VM.read mv (fromIntegral w)
         VM.write mv (fromIntegral w) ((wl,d):el)
  mapM_ go l
  return mv

-- | Some instructions have multiple interpretations depending on the
-- size of the operands.  This is represented by multiple instructions
-- with the same opcode distinguished by the /o= and /a= annotations.
-- This function removes those definitions which don't match the given
-- prefix.
validPrefix :: Prefixes -> Def -> Bool
validPrefix pfx d = matchRequiredOpSize pfx d
                 && matchReqAddr (prAddrSize pfx) d
  where
    matchReqAddr sz = maybe True (==sz) . view reqAddrSize

-- | This function calculates all possible byte sequences allowed for
-- a particular instruction, along with the resulting Prefixes structure
--
-- There are 4 classes of prefixes, which can occur in any order,
-- although not all instructions support all prefixes.  There is
-- also the REX prefix which extends the register ids.
--
-- LOCK group has rep, repz
-- SEG group has overrides for all 6 segments
-- ASO has a single member
-- OSO has a single member
-- REX has up to 4 bits, dep. on the instruction.
--
-- Each prefix is optional.  Some SSE instructions require a certain prefix, but
-- seem to allow other prefixes sometimes, along with REX.
--

-- | A function which given an assignment of viewed prefixes, returns the modifies
-- set based on a particular value seen.
type PrefixAssignFun = Prefixes -> Prefixes

-- | Map prefix bytes corresponding to prefixes to the associated update function.
type PrefixAssignTable = HM.HashMap Word8 PrefixAssignFun

-- Given a list of allowed prefixes
simplePrefixes :: BS.ByteString -> [String] -> PrefixAssignTable
simplePrefixes mnem allowed
  | "rep" `elem` allowed && "repz" `elem` allowed = error $
      "Instruction " ++ BSC.unpack mnem ++ " should not be allowed to have both rep and repz as prefixes"
  | otherwise = HM.fromList [ v | (name, v) <- simplePrefixBytesFuns, name `elem` allowed ]

simplePrefixBytesFuns :: [(String, (Word8, PrefixAssignFun))]
simplePrefixBytesFuns =
  [ ("lock",  (0xf0, set prLockPrefix LockPrefix))
  , ("repnz", (0xf2, set prLockPrefix RepNZPrefix))
  , ("repz",  (0xf3, set prLockPrefix RepZPrefix))
  , ("rep",   (0xf3, set prLockPrefix RepPrefix))
  , ("oso",   (0x66, set prOSO True))
  , ("aso",   (0x67, set prASO True))
  ]

-- | The simple \"legacy\" prefixes.
simplePrefixBytes :: HS.HashSet Word8
simplePrefixBytes = HS.fromList $ map (fst . snd) simplePrefixBytesFuns

-- | Table for segment prefixes
segPrefixes :: [String] -> PrefixAssignTable
segPrefixes allowed
  | "seg" `elem` allowed = HM.fromList [ (x, set prSP (SegmentPrefix x)) | x <- segPrefixBytesList ]
  | otherwise            = HM.empty

-- | The segment prefixes.
segPrefixBytes :: HS.HashSet Word8
segPrefixBytes = HS.fromList segPrefixBytesList

segPrefixBytesList :: [Word8]
segPrefixBytesList = [0x26, 0x2e, 0x36, 0x3e, 0x64, 0x65]

-- FIXME: we could also decode the REX
rexPrefixes :: [String] -> PrefixAssignTable
rexPrefixes allowed
  | null possibleBits = HM.empty
  | otherwise         = HM.fromList
                          [ (x, set prREX (REX x))
                          | xs <- subsequences possibleBits
                          , let x = foldl (.|.) rex_instr_pfx xs ]
  where
    possibleBits  = [ b | (name, b) <- rexPrefixBits, name `elem` allowed ]
    rexPrefixBits = [ ("rexw", rex_w_bit)
                    , ("rexr", rex_r_bit)
                    , ("rexx", rex_x_bit)
                    , ("rexb", rex_b_bit) ]

-- | The REX prefix bytes.
rexPrefixBytes :: HS.HashSet Word8
rexPrefixBytes = HS.fromList
  [ foldl (.|.) rex_instr_pfx xs
  | xs <- subsequences [ rex_w_bit, rex_r_bit, rex_x_bit, rex_b_bit ]
  ]

-- | `notrack` prefix bytes overlap with `seg` prefix bytes, but in practice
-- should be on strictly distinct sets of opcodes.
notrackPrefixByte :: Word8
notrackPrefixByte = 0x3e

notrackPrefixBytes :: HS.HashSet Word8
notrackPrefixBytes = HS.singleton notrackPrefixByte

-- | Table for notrack prefixes
notrackPrefix :: [String] -> PrefixAssignTable
notrackPrefix allowed
  | "notrack" `elem` allowed = HM.singleton notrackPrefixByte (set prNoTrack True)
  | otherwise                = HM.empty

-- | All prefix bytes besides the VEX ones. (See @Note [x86_64 disassembly]@
-- for why VEX prefix bytes are treated specially.)
nonVexPrefixBytes :: HS.HashSet Word8
nonVexPrefixBytes = HS.unions
  [ simplePrefixBytes
  , segPrefixBytes
  , rexPrefixBytes
  , notrackPrefixBytes
  ]

-- | Given a 'Def', construct all possible combinations of bytes representing
-- valid VEX prefix and opcode bytes for that instruction. (See
-- @Note [x86_64 disassembly]@ for why we care about VEX prefix bytes in
-- particular.) Each element of the returned list consists of a pair where
-- the first part of the pair contains the raw bytes, and the second part of
-- the pair contains the corresponding 'VEX' (if one exists) and 'Def'.
allVexPrefixesAndOpcodes :: Def -> [([Word8], (Maybe VEX, Def))]
allVexPrefixesAndOpcodes def
  | null (def ^. vexPrefixes)
  = [ (def^.defOpcodes, (Nothing, def)) ]

  | otherwise
  = [ (vexBytes ++ def^.defOpcodes, (Just vex, def))
    | (vexBytes, vex) <- mkVexPrefixes def ]
  where
    mkVexPrefixes :: Def -> [([Word8], VEX)]
    mkVexPrefixes df = map cvt (df ^. vexPrefixes)
      where
      cvt pref =
        case pref of
          [ _, b ]      -> (pref, VEX2 b)
          [ _, b1, b2 ] -> (pref, VEX3 b1 b2)
          _             -> error "mkVexPrefixes: unexpected byte sequence"

-- | Given a list of instruction 'Def's, compute a lookup table for its VEX
-- prefix and opcode bytes. See @Note [x86_64 disassembly]@ for more details on
-- how this works.
mkOpcodeTable :: [Def] -> ParserGen OpcodeTable
mkOpcodeTable defs = go (concatMap allVexPrefixesAndOpcodes defs)
  where -- Recursive function that generates opcode table by parsing
        -- opcodes in first element of list.
        go :: -- Potential opcode definitions with the remaining opcode
              -- bytes each potential definition expects.
              [([Word8], (Maybe VEX, Def))]
           -> ParserGen OpcodeTable
        go l
           -- If we have parsed all the opcodes expected by the remaining
           -- definitions.
          | all opcodeDone l =
              case partition (expectsModRM.snd) (snd <$> l) of
                (defsWithModRM, defsWithoutModRM) ->
                  pure $! OpcodeTableEntry defsWithModRM defsWithoutModRM
            -- If we still have opcodes to parse, generate table for next
            -- opcode match.
          | otherwise = do
            let v = partitionBy l
                g i = go (v V.! i)
            tbl <- V.generateM 256 g
            pure $! OpcodeTable tbl
        -- Return whether opcode parsing is done.
        opcodeDone :: ([Word8], a) -> Bool
        opcodeDone (remaining,_) = null remaining

requireModCheck :: Def -> Bool
requireModCheck d = isJust (d^.requiredMod)

mkModTable :: Monad m => DefTableFn m ModTable
mkModTable defs
  | any (requireModCheck . snd) defs = do
    let memDef d =
          case d^.requiredMod of
            Just OnlyReg -> False
            _ -> none modRMRegOperand (d^.defOperands)
        modRMRegOperand nm =
          case nm of
            OpType sc _ ->
              case sc of
                ModRM_rm_mod3 -> True
                _ -> False
            MXRX _ _  -> True
            RG_MMX_rm -> True -- FIXME: no MMX_reg?
            RG_XMM_rm {} -> True
            RG_ST _   -> True
            _         -> False
    let regDef d =
          case d^.requiredMod of
            Just OnlyMem -> False
            _ -> none modRMMemOperand (d^.defOperands)
        modRMMemOperand nm =
          case nm of
            OpType sc _ ->
              case sc of
                ModRM_rm -> True
                _ -> False
            M_FP    -> True
            M       -> True
            M_X _   -> True
            M_FloatingPoint _ -> True
            MXRX _ _ -> True
            RM_MMX  -> True
            RM_XMM {} -> True
            _       -> False
    memTbl <- checkRequiredRM (filter (memDef . snd) defs)
    regTbl <- checkRequiredRM (filter (regDef . snd) defs)
    pure $! ModTable memTbl regTbl
  | otherwise = do
    tbl <- checkRequiredRM defs
    pure $! ModUnchecked tbl

checkRequiredReg :: Monad m => DefTableFn m (RegTable ModTable)
checkRequiredReg defs
  | any (\(_,d) -> isJust (d^.requiredReg)) defs = do
    let p i (_,d) = equalsOptConstraint i (d^.requiredReg)
        eltFn i = mkModTable $ filter (p i) defs
    v <- mkFin8Vector eltFn
    pure $! RegTable v
  | otherwise = do
      tbl <- mkModTable defs
      pure $! RegUnchecked tbl

-- | Make a vector with length 8 from a function over fin8.
mkFin8Vector :: Monad m => (Fin8 -> m v) -> m (V.Vector v)
mkFin8Vector f = do
  let g i =
        let j = case asFin8 (fromIntegral i) of
                  Just j' -> j'
                  Nothing -> error $ "internal: Expected number between 0-7, received " ++ show i
         in f j
  V.generateM 8 g

-- | Return true if value matches an optional constraint (where 'Nothing' denotes any)
-- and 'Just v' denotes a singleton v.
equalsOptConstraint :: Eq a => a -> Maybe a -> Bool
equalsOptConstraint _ Nothing = True
equalsOptConstraint i (Just c) = i == c

-- | Return true if the definition matches the Fin8 constraint
matchRMConstraint :: Fin8 -> (Maybe VEX,Def) -> Bool
matchRMConstraint i (_,d) = i `equalsOptConstraint` (d^.requiredRM)

-- | Check required RM if needed, then forward to final table.
checkRequiredRM :: Monad m => DefTableFn m RMTable
checkRequiredRM defs
    -- Split on the required RM value if any of the definitions depend on it
  | any (\(_,d) -> isJust (d^.requiredRM)) defs =
    let eltFn i = pure $ filter (i `matchRMConstraint`) defs
     in RegTable <$> mkFin8Vector eltFn
  | otherwise = pure $ RegUnchecked defs

------------------------------------------------------------------------
-- Parsing

-- | Read a ModRM byte.
readModRM :: ByteReader m => m ModRM
readModRM = ModRM <$> readByte

-- | Read a SIB byte.
readSIB :: ByteReader m => m SIB
readSIB = SIB <$> readByte

-- | Read 32bit value
read_disp32 :: ByteReader m => m Displacement
read_disp32 = Disp32 <$> readDImm

-- | Read an 8bit value and sign extend to 32-bits.
read_disp8 :: ByteReader m => m Displacement
read_disp8 = Disp8 <$> readSByte

parseReadTable :: ByteReader m
               => [Word8]
               -> ModRM
               -> [(Maybe VEX, Def)]
               -> m InstructionInstance
parseReadTable prefixBytes modRM dfs = do
  (pfx, df) <- matchDefWithPrefixBytes prefixBytes dfs
  let osz = prefixOperandSizeConstraint pfx df
  let finish args =
        II
          { iiLockPrefix = pfx ^. prLockPrefix,
            iiAddrSize = prAddrSize pfx,
            iiOp = df^.defMnemonic,
            iiArgs = args,
            iiPrefixes = pfx,
            iiRequiredPrefix = view requiredPrefix df,
            iiOpcode = view defOpcodes df,
            iiRequiredMod = view requiredMod df,
            iiRequiredReg = view requiredReg df,
            iiRequiredRM = view requiredRM df
          }
  finish <$> traverse (parseValueType pfx osz (Just modRM)) (view defOperands df)

getReadTable ::
  ModRM ->
  RMTable ->
  [(Maybe VEX, Def)]
getReadTable modRM (RegTable v) = v V.! fromIntegral (modRM_rm modRM)
getReadTable _modRM (RegUnchecked m) = m

getRMTable ::
  ModRM ->
  ModTable ->
  RMTable
getRMTable modRM mtbl =
  case mtbl of
    ModTable x y
      | modRM_mod modRM == 3 -> y
      | otherwise -> x
    ModUnchecked x -> x

-- | The state of what prefixes have been seen during prefix byte validation.
-- Note that this data type is only used in 'validatePrefixBytes'.
data ValidatePrefixState = ValidatePrefixState
  { _seenRequiredPrefix :: Bool
  , _seenREX :: Bool
  , _prefixAssignFunMap :: HM.HashMap Word8 PrefixAssignFun
    -- ^ For each prefix byte that we have seen, record what effect it has on
    -- a 'Prefixes' value. At the end of 'validatePrefixBytes', all of the
    -- recorded 'PrefixAssignFun's will be applied to construct the final
    -- 'Prefixes' value.
  }

-- | The starting state at the beginning of prefix byte validation (see
-- 'validatePrefixBytes').
defaultValidatePrefixState :: ValidatePrefixState
defaultValidatePrefixState = ValidatePrefixState
  { _seenRequiredPrefix = False
  , _seenREX = False
  , _prefixAssignFunMap = HM.empty
  }

seenRequiredPrefix :: Lens' ValidatePrefixState Bool
seenRequiredPrefix = lens _seenRequiredPrefix (\s v -> s { _seenRequiredPrefix = v })

seenREX :: Lens' ValidatePrefixState Bool
seenREX = lens _seenREX (\s v -> s { _seenREX = v })

prefixAssignFunMap :: Lens' ValidatePrefixState (HM.HashMap Word8 PrefixAssignFun)
prefixAssignFunMap = lens _prefixAssignFunMap (\s v -> s { _prefixAssignFunMap = v })

-- | A monad (internal to 'validatePrefixBytes') used for validating prefix bytes.
newtype ValidatePrefixM a =
    ValidatePrefixM (ExceptT String (State ValidatePrefixState) a)
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadState ValidatePrefixState
           )

evalValidatePrefixM :: ValidatePrefixState -> ValidatePrefixM a -> Either String a
evalValidatePrefixM st (ValidatePrefixM ma) = evalState (runExceptT ma) st

-- | Given a set of prefix bytes and a 'Def', check to see if the 'Def'
-- actually accepts the prefixes. If so, return @'Right' (pfx, def)@, where
-- @pfx@ is a 'Prefixes' value representing the prefix bytes and @def@ is the
-- input 'Def', possibly with some minor fixups applied in certain special
-- cases. (See @Note [x86_64 disassembly]@ for more about these special cases
-- and corresponding fixups.) Otherwise, return @'Left' err@, where @err@
-- describes why the prefixes were not valid for that instruction.
--
-- Note that this list of checks is almost certainly incomplete. We aim to only
-- include only those checks that are actually necessary to disambiguate 'Def's
-- with identical opcodes, and no more than that.
validatePrefixBytes :: [Word8] -> Maybe VEX -> Def
                    -> Either String (Prefixes, Def)
validatePrefixBytes prefixBytes mbVex def =
  evalValidatePrefixM defaultValidatePrefixState (go prefixBytes)
  where
    go :: [Word8] -> ValidatePrefixM (Prefixes, Def)
    go (b:bs)
      | def^.requiredPrefix == Just b
      = do seenRequiredPrefix .= True
           -- Required prefix doesn't do anything, but must occur
           prefixAssignFunMap %= HM.insert b id
           go bs

      | Just fun <- HM.lookup b rexPfxs
      = do seenREX .= True
           prefixAssignFunMap %= HM.insert b fun
           go bs

      | Just fun <- HM.lookup b simplePfxs
      = do prefixAssignFunMap %= HM.insert b fun
           go bs

      | Just fun <- HM.lookup b segPfxs
      = do prefixAssignFunMap %= HM.insert b fun
           go bs

      | Just fun <- HM.lookup b notrackPfx
      = do prefixAssignFunMap %= HM.insert b fun
           go bs

      | otherwise
      = throwError $ unlines
          [ "Encountered a " ++ BSC.unpack mnem ++ " instruction " ++
              "with a " ++ show b ++ " prefix byte,"
          , "which is not permitted"
          ]

    go [] = do
      st <- get
      let pfx = appList (HM.elems (st^.prefixAssignFunMap))
                        (set prVEX mbVex defaultPrefix)
      (pfx', def') <-
        -- Here is where we perform special cases of instructions that are
        -- similar to nop (opcode 0x90).
        if |  -- If we have a nop with an OSO prefix, what we really have is an
              -- xchg instruction.
              def^.defOpcodes == [0x90] && pfx^.prOSO
           -> do let xchgMnemonic = "xchg"
                 let mbOprs = do
                       -- The operand types come from here:
                       -- https://github.com/vmt/udis86/blob/56ff6c87c11de0ffa725b14339004820556e343d/docs/x86/optable.xml#L8447
                       opr1 <- lookupOperandType xchgMnemonic "R0v"
                       opr2 <- lookupOperandType xchgMnemonic "rAX"
                       pure [opr1, opr2]
                 case mbOprs of
                   Just oprs -> pure ( pfx & prOSO .~ False
                                     , def & defMnemonic .~ xchgMnemonic
                                           & defOpcodes  %~ (0x66:)
                                           & defOperands .~ oprs
                                     )
                   Nothing -> impossible "Could not find operand types for R0v and rAX"

           |  -- If we have a nop with a REP prefix, what we really have is a
              -- pause instruction.
              def^.defOpcodes == [0x90] && pfx^.prLockPrefix == RepPrefix
           -> pure ( pfx & prLockPrefix .~ NoLockPrefix
                   , def & defMnemonic .~ "pause"
                         & defOpcodes  %~ (0xf3:)
                   )

           |  -- We have to lop off the 0xf3 part of the opcode for endbr32 and
              -- endbr64 to avoid ambiguity with REP prefixes, so add the 0xf3
              -- bit back to the opcode post facto.
              mnem `elem` ["endbr32", "endbr64"]
           ,  Just reqPfx <- def^.requiredPrefix
           -> pure ( pfx
                   , def & defOpcodes     %~ (reqPfx:)
                         & requiredPrefix .~ Nothing
                   )

           |  otherwise
           -> pure (pfx, def)
      if |  Just reqPfx <- def'^.requiredPrefix
         ,  not (st^.seenRequiredPrefix)
         -> throwError $ unlines
              [ "The " ++ BSC.unpack mnem ++ " instruction has a required " ++
                  "prefix of " ++ show reqPfx ++ ","
              , "but the disassembled prefixes did not contain it"
              ]

         |  -- Having both REX and VEX prefixes is #UD.
            st^.seenREX, isJust mbVex
         -> impossible $
              BSC.unpack mnem ++ " instruction encountered with both " ++
              "REX and VEX prefixes"

         |  validPrefix pfx' def'
         -> pure (pfx', def')

         |  otherwise
         -> throwError $ unlines
              [ "Could not validate the " ++ BSC.unpack mnem ++ " instruction " ++
                  "against the following bytes: "
              , show prefixBytes
              ]

    appList :: [a -> a] -> a -> a
    appList = foldl (.) id

    mnem = def^.defMnemonic
    -- Get list of permitted prefixes
    allowed = def^.defPrefix
    -- Get all subsets of allowed segmented prefixes
    segPfxs :: PrefixAssignTable
    segPfxs = segPrefixes allowed
    -- Get all subsets of allowed legacy prefixes
    simplePfxs :: PrefixAssignTable
    simplePfxs = simplePrefixes mnem allowed
    -- Get all subsets of allowed REX prefixes
    rexPfxs :: PrefixAssignTable
    rexPfxs = rexPrefixes allowed
    notrackPfx :: PrefixAssignTable
    notrackPfx = notrackPrefix allowed

    defaultPrefix = Prefixes { _prLockPrefix = NoLockPrefix
                             , _prSP  = no_seg_prefix
                             , _prREX = no_rex
                             , _prVEX = Nothing
                             , _prASO = False
                             , _prOSO = False
                             , _prNoTrack = False
                             }

    -- Reserved for situations that should never happen
    impossible = error

-- | What can go wrong when searching for an instruction that validates against
-- a set of prefix bytes.
data DefSearchError
  = -- | No instruction was found. This probably occurs due to the instruction
    -- containing invalid bytes.
    NoDefFound
  | -- | Multiple possible instructions were found, resulting in an ambiguous
    -- parse.
    MultipleDefsFound [String] -- The names of each found instruction

-- | Given a list of prefix bytes and a set of candidate instruction
-- definitions, find a candidate for which the prefix bytes are valid. If
-- exactly one such candidate can be found, return 'Right'. If no candidate
-- is found or multiple candidates are found (i.e., an ambiguity is
-- encountered), return 'Left'.
findDefWithPrefixBytes :: [Word8]
                       -> [(Maybe VEX, Def)]
                       -> Either DefSearchError (Prefixes, Def)
findDefWithPrefixBytes prefixBytes defs =
  case mapMaybe match defs of
    [pfxdef]    -> Right pfxdef
    []          -> Left NoDefFound
    res@(_:_:_) -> Left $ MultipleDefsFound
                        $ map (BSC.unpack . view defMnemonic . snd) res
  where
    match :: (Maybe VEX, Def) -> Maybe (Prefixes, Def)
    match (mbVex, def) =
      case validatePrefixBytes prefixBytes mbVex def of
        Left _err    -> Nothing
        Right pfxdef -> Just pfxdef

-- | Return the instruction that matches the set of prefix bytes from a set of
-- candidates, throwing an error if that instruction cannot be found.
matchDefWithPrefixBytes :: ByteReader m
                        => [Word8]
                        -> [(Maybe VEX, Def)]
                        -> m (Prefixes, Def)
matchDefWithPrefixBytes prefixBytes defs =
  case findDefWithPrefixBytes prefixBytes defs of
    Right pfxdef -> pure pfxdef
    Left NoDefFound -> invalidInstruction
    Left (MultipleDefsFound defNms) -> error $ unlines $
      ("Multiple instructions match: " ++ show prefixBytes) : defNms

-- | Parse instruction using byte reader.
disassembleInstruction :: forall m
                        . ByteReader m
                       => NextOpcodeTable
                       -> m InstructionInstance
disassembleInstruction tr0 = loopPrefixBytes Seq.empty
  where
    -- Parse as many bytes as possible that are known to be prefix bytes. Once
    -- we encounter the first byte that isn't a prefix byte, move on to parsing
    -- opcode bytes.
    loopPrefixBytes :: Seq.Seq Word8 -> m InstructionInstance
    loopPrefixBytes prefixBytes = do
      b <- readByte
      if |  HS.member b nonVexPrefixBytes
         -> loopPrefixBytes (prefixBytes Seq.|> b)

         |  otherwise
         -> loopOpcodes (F.toList prefixBytes) tr0 b

    -- Parse opcode byte and use them to navigate through the opcode table.
    -- Once we have parsed all of the opcode bytes in an instruction,
    -- disassemble the rest of the instruction.
    loopOpcodes :: [Word8]
                -> NextOpcodeTable
                -> Word8
                -> m InstructionInstance
    loopOpcodes prefixBytes = go
      where
        go :: NextOpcodeTable -> Word8 -> m InstructionInstance
        go tr opcodeByte =
          case tr V.! fromIntegral opcodeByte of
            OpcodeTable tr' -> do
              opcodeByte' <- readByte
              go tr' opcodeByte'
            OpcodeTableEntry defsWithModRM defsWithoutModRM
              |  -- Check the instruction candidates without a ModR/M byte
                 -- first. If one is found, there is an invariant that none
                 -- of the instruction candidates without a ModR/M byte
                 -- should validate with the set of parsed prefixes.
                 Right (pfx, def) <- findDefWithPrefixBytes prefixBytes defsWithoutModRM
              -> assert (all (\(mbVex, df) ->
                               isLeft $ validatePrefixBytes prefixBytes mbVex df)
                             defsWithModRM) $
                 disassembleWithoutModRM def pfx

              |  otherwise
              -> disassembleWithModRM prefixBytes defsWithModRM

    -- Disassemble instruction candidates with a ModR/M byte.
    disassembleWithModRM :: [Word8]
                         -> [(Maybe VEX, Def)]
                         -> m InstructionInstance
    disassembleWithModRM prefixBytes defs = do
      tbl <- checkRequiredReg defs
      modRM <- readModRM
      case tbl of
        RegTable v ->
          let mtbl = v V.! fromIntegral (modRM_reg modRM) in
          parseReadTable prefixBytes modRM (getReadTable modRM (getRMTable modRM mtbl))
        RegUnchecked mtbl ->
          parseReadTable prefixBytes modRM (getReadTable modRM (getRMTable modRM mtbl))

    -- Disassemble an instruction without a ModR/M byte.
    disassembleWithoutModRM :: Def
                            -> Prefixes
                            -> m InstructionInstance
    disassembleWithoutModRM df pfx =
      let osz = prefixOperandSizeConstraint pfx df in
      finish <$> traverse (parseValueType pfx osz Nothing) (df^.defOperands)
      where finish args =
              II { iiLockPrefix = pfx^.prLockPrefix
                 , iiAddrSize = prAddrSize pfx
                 , iiOp   = df^.defMnemonic
                 , iiArgs = args
                 , iiPrefixes = pfx
                 , iiRequiredPrefix = view requiredPrefix df
                 , iiOpcode = view defOpcodes df
                 , iiRequiredMod = view requiredMod df
                 , iiRequiredReg = view requiredReg df
                 , iiRequiredRM = view requiredRM df
                 }

-- | Returns the size of a function.
sizeFn :: OperandSizeConstraint -> OperandSize -> SizeConstraint
sizeFn _ BSize = error "internal: sizeFn given BSize"
sizeFn _ WSize = Size16
sizeFn _ DSize = Size32
sizeFn _ QSize = Size64
sizeFn OpSize16 VSize   = Size16
sizeFn OpSize32 VSize   = Size32
sizeFn OpSize64 VSize   = Size64
sizeFn _        OSize   = Size128
sizeFn _        QQSize  = Size256
sizeFn OpSize64 YSize   = Size64
sizeFn _        YSize   = Size32
sizeFn OpSize16 ZSize   = Size16
sizeFn _        ZSize   = Size32
sizeFn _        RDQSize = Size64

regSizeFn :: OperandSizeConstraint -> REX -> OperandSize -> Word8 -> Value
regSizeFn _ rex BSize = ByteReg . reg8 rex
regSizeFn osz _ sz =
  case sizeFn osz sz of
    Size16 -> WordReg  . Reg16
    Size32 -> DWordReg . Reg32
    Size64 -> QWordReg . Reg64
    Size128  -> error "Unexpected register size function: Size128"
    Size256  -> error "Unexpected register size function: Size256"

memSizeFn :: OperandSizeConstraint -> OperandSize -> AddrRef -> Value
memSizeFn _ BSize = Mem8
memSizeFn osz sz =
  case sizeFn osz sz of
    Size16 -> Mem16
    Size32 -> Mem32
    Size64 -> Mem64
    Size128 -> Mem128
    Size256 -> Mem256


getREX :: Prefixes -> REX
getREX p = case p ^. prVEX of
             Just vex -> vex ^. vexRex
             _        -> p ^. prREX

readOffset :: ByteReader m => Segment -> Bool -> m AddrRef
readOffset s aso
  | aso       = Offset_32 s <$> readDImm
  | otherwise = Offset_64 s <$> readQWord

parseValue :: (ByteReader m, HasCallStack)
           => Prefixes
           -> OperandSizeConstraint -- ^ Operand size
           -> Maybe ModRM
           -> OperandType
           -> m Value
parseValue p osz mmrm tp = do
  let sp  = p^.prSP
      rex = getREX p
      aso = p^.prASO
      msg = "internal: parseValue missing modRM with operand type: " ++ show tp
      modRM = fromMaybe (error msg) mmrm -- laziness is used here and below, this is not used for e.g. ImmediateSource

  let reg = modRM_reg modRM
  let reg_with_rex :: Word8
      reg_with_rex = rex_r rex .|. reg
      -- This reads an address reference
      addr :: ByteReader m => m AddrRef
      addr = case modRM_mod modRM of
               0 -> readNoOffset p modRM
               1 -> readWithOffset read_disp8  aso p modRM
               2 -> readWithOffset read_disp32 aso p modRM
               unknownrm ->
                 -- This case has been observed to be 3 in the wild, but it is
                 -- likely that it was due to decoding an invalid instruction
                 -- during code discovery
                 fail $ "internal: parseValue given modRM to register with operand type: " ++ show tp ++ " at value " ++ show unknownrm
      rm_reg :: Word8
      rm_reg = rex_b rex .|. modRM_rm modRM

  case tp of
    AbsoluteAddr -> error $ "Absolute addr not yet supported."
    OpType ModRM_rm sz
      | modRM_mod modRM == 3 -> pure $ regSizeFn osz rex sz rm_reg
      | otherwise            -> memSizeFn osz sz <$> addr
    OpType ModRM_rm_mod3 sz
      | modRM_mod modRM == 3 -> pure $ regSizeFn osz rex sz rm_reg
      | otherwise -> fail "Unexpected memory operand in parseValue"
    OpType ModRM_reg sz -> pure $ regSizeFn osz rex sz reg_with_rex
    OpType (Opcode_reg r) sz -> pure $ regSizeFn osz rex sz (rex_b rex .|. r)
    OpType (Reg_fixed r) sz  -> pure $ regSizeFn osz rex sz r
    OpType VVVV sz
      | Just vx <- p ^. prVEX -> pure $ regSizeFn osz rex sz $ complement (vx ^. vexVVVV) .&. 0xF
      | otherwise -> error "[VVVV_XMM] Missing VEX prefix "
    OpType ImmediateSource BSize ->
      ByteImm <$> readByte
    OpType ImmediateSource WSize ->
       WordImm  <$> readWord
    OpType ImmediateSource VSize ->
      case osz of
        OpSize16 ->  WordImm <$> readWord
        OpSize32 -> DWordImm <$> readDImm
        OpSize64 -> QWordImm <$> readQUImm
    OpType ImmediateSource ZSize ->
      case osz of
        OpSize16 ->  WordImm <$> readWord
        _        -> DWordImm <$> readDImm
    OpType ImmediateSource _ ->
      error "Unsupported immediate source."
    OpType OffsetSource BSize ->
      Mem8 <$> readOffset (sp `setDefault` DS) aso
    OpType OffsetSource VSize ->
      case osz of
        OpSize16 -> Mem16 <$> readOffset (sp `setDefault` DS) aso
        OpSize32 -> Mem32 <$> readOffset (sp `setDefault` DS) aso
        OpSize64 -> Mem64 <$> readOffset (sp `setDefault` DS) aso
    OpType OffsetSource sz ->
      error $ "Unsupported offset source size: " ++ show sz
    OpType JumpImmediate BSize ->
      JumpOffset JSize8 <$> readJump JSize8
    OpType JumpImmediate ZSize ->
      if osz == OpSize16 then
        fmap (JumpOffset JSize16) $ readJump JSize16
      else
        fmap (JumpOffset JSize32) $ readJump JSize32
    OpType JumpImmediate sz ->
      error $ "Unsupported jump immediate size: " ++ show sz
    RG_C   -> return $ ControlReg (controlReg reg_with_rex)
    RG_dbg -> return $ DebugReg   (debugReg   reg_with_rex)
    RG_S   -> SegmentValue <$> segmentRegisterByIndex reg
    RG_ST n -> return $ X87Register n
    RG_MMX_reg -> return $ MMXReg (mmxReg reg)

    RG_XMM_reg mb -> return $ largeReg p mb reg_with_rex
    RG_XMM_rm  mb -> return $ largeReg p mb rm_reg
    RM_XMM mb
      | modRM_mod modRM == 3 -> pure $ largeReg p mb rm_reg

      | Just OSize  <- mb -> Mem128 <$> addr
      | Just QQSize <- mb -> Mem256 <$> addr
      | Just sz <- mb     -> error ("[RM_XMM] Unexpected size: " ++ show sz)

      | Nothing <- mb
      , Just vx <- p ^. prVEX
      , vx ^. vex256      -> Mem256 <$> addr

      | otherwise         -> Mem128 <$> addr

    VVVV_XMM mb
      | Just vx <- p ^. prVEX ->
          return $ largeReg p mb $ complement (vx ^. vexVVVV) .&. 0xF
      | otherwise -> error "[VVVV_XMM] Missing VEX prefix "

    SEG s -> return $ SegmentValue s
    M_FP -> FarPointer <$> addr
    M    ->    VoidMem <$> addr
    M_FloatingPoint sz ->
      case sz of
        FPSize32 -> FPMem32 <$> addr
        FPSize64 -> FPMem64 <$> addr
        FPSize80 -> FPMem80 <$> addr
    M_U sz
      | modRM_mod modRM == 3 ->
        return $ XMMReg (xmmReg rm_reg)
      | otherwise ->
        memSizeFn osz sz <$> addr
    M_X sz -> memSizeFn osz sz <$> addr
    MXRX msz rsz
      | modRM_mod modRM == 3 ->
        case sizeFn osz rsz of
          Size16  -> pure $  WordReg $ Reg16 rm_reg
          Size32  -> pure $ DWordReg $ Reg32 rm_reg
          Size64  -> pure $ QWordReg $ Reg64 rm_reg
          Size128 -> error "128-bit registers are not supported."
          Size256 -> error "256-bit registers are not supported."
      | otherwise -> memSizeFn osz msz <$> addr -- FIXME!!
    RM_MMX
      | modRM_mod modRM == 3 ->
        pure $ MMXReg $ mmxReg $ modRM_rm modRM
      | otherwise -> Mem64 <$> addr
    RG_MMX_rm -> assert (modRM_mod modRM == 3) $ do
      pure $ MMXReg $ mmxReg $ modRM_rm modRM
    M_Implicit seg r sz -> do
      let a | aso       = Addr_32 seg (Just (Reg32 (reg64No r))) Nothing NoDisplacement
            | otherwise = Addr_64 seg (Just r)                   Nothing NoDisplacement
      pure $ memSizeFn OpSize64 sz a
    IM_1 -> pure $ ByteImm 1
    IM_SB -> ByteSignedImm <$> readSByte
    IM_SZ ->
      -- This reads 16-bits if operand size is 16bits and 32-bits otherwise.
      case osz of
        OpSize16  ->  WordSignedImm <$> readSWord
        _         -> DWordSignedImm <$> readSDWord


parseValueType ::
  (ByteReader m, HasCallStack) =>
  Prefixes ->
  OperandSizeConstraint ->
  Maybe ModRM ->
  OperandType ->
  m (Value, OperandType)
parseValueType p osz mmrm tp = (\v -> (v,tp)) <$> parseValue p osz mmrm tp

largeReg :: Prefixes -> Maybe OperandSize -> Word8 -> Value
largeReg p mb num =
  case mb of

    Nothing | Just vx <- p ^. prVEX
            , vx ^. vex256  -> useYMM
            | otherwise     -> useXMM

    Just sz ->
      case sz of
        OSize  -> useXMM
        QQSize -> useYMM
        _      -> error ("[largeReg] Unexpected size: " ++ show sz)

  where
  useXMM = XMMReg (xmmReg num)
  useYMM = YMMReg (ymmReg num)

readNoOffset :: ByteReader m
             => Prefixes
             -> ModRM
             -> m AddrRef
readNoOffset p modRM = do
  let aso = p^.prASO
  let rm = modRM_rm modRM
  let rex = getREX p
      sp = p^.prSP
  let base_reg  = (rex_b rex .|.)
      index_reg = (rex_x rex .|.)
  if rm == rsp_idx then do
    sib <- readSIB
    let base = sib_base sib
    let si = sib_si index_reg sib
    if base == rbp_idx then do
      memRef aso (sp `setDefault` DS) Nothing si <$> read_disp32
     else do
      let seg | base == rsp_idx = sp `setDefault` SS
              | otherwise       = sp `setDefault` DS
      return $ memRef aso seg (Just (base_reg base)) si NoDisplacement
  else if rm == rbp_idx then do
    let seg = sp `setDefault` SS
    disp <- read_disp32
    pure $!
      if aso then
        IP_Offset_32 seg disp
       else
        IP_Offset_64 seg disp
  else do
    let seg = sp `setDefault` DS
    return $ memRef aso seg (Just (base_reg rm)) Nothing NoDisplacement

readWithOffset :: ByteReader m
               => m Displacement
               -> Bool -- ^ Address size override
               -> Prefixes
               -> ModRM
               -> m AddrRef
readWithOffset readDisp aso p modRM = do
  let sp = p^.prSP
  let rex = getREX p
  let rm = modRM_rm modRM
  let base_reg  = (rex_b rex .|.)
      index_reg = (rex_x rex .|.)
  (base, si) <-
      if rm == rsp_idx then do
        sib <- readSIB
        return (sib_base sib, sib_si index_reg sib)
      else
        return (rm, Nothing)
  let seg | base == rsp_idx = SS
          | base == rbp_idx = SS
          | otherwise       = DS
  o <- readDisp
  return $ memRef aso (sp `setDefault` seg) (Just (base_reg base)) si o

-- | Information about a disassembled instruction at a given address
data DisassembledAddr = DAddr { disOffset :: Int
                              , disLen :: Int
                              , disInstruction :: Maybe InstructionInstance
                              }
                              deriving Show

-- | Try disassemble returns the numbers of bytes read and an instruction instance.
tryDisassemble :: NextOpcodeTable -> BS.ByteString -> (Int, Maybe InstructionInstance)
tryDisassemble p bs0 = decode bs0 $ runGetIncremental (disassembleInstruction p)
  where bytesRead bs = BS.length bs0 - BS.length bs

        -- Decode instructions.
        decode :: BS.ByteString
               -> Decoder InstructionInstance
               -> (Int, Maybe InstructionInstance)
        decode _ (Fail bs' _ _) = (bytesRead bs', Nothing)
        -- End the recursive decoding when the input is empty. This prevents a loop.
        decode bs (Partial _) | BS.null bs = (bytesRead bs, Nothing)
        decode bs (Partial f) = decode BS.empty (f (Just bs))
        decode _ (Done bs' _ i) = (bytesRead bs', Just i)

-- | Parse the buffer as a list of instructions.
--
-- Returns a list containing offsets,
disassembleBuffer :: NextOpcodeTable
                  -> BS.ByteString
                     -- ^ Buffer to decompose
                  -> [DisassembledAddr]
disassembleBuffer p bs0 = group 0 (decode bs0 decoder)
  where decoder = runGetIncremental (disassembleInstruction p)

        -- Group continuous regions that cannot be disassembled together.
        group :: Int -> [(Int, Maybe InstructionInstance)] -> [DisassembledAddr]
        group o ((i,Nothing):(j,Nothing):r) = group o ((i+j,Nothing):r)
        group o ((i,mv):r) = DAddr o i mv:group (o+i) r
        group _ [] = []

        -- Decode instructions.
        decode :: BS.ByteString
               -> Decoder InstructionInstance
               -> [(Int, Maybe InstructionInstance)]
        decode bs d =
          case BS.uncons bs of
            Nothing -> []
            Just (_, bsRest) ->
              case d of
                Fail{}       -> (1, Nothing):decode bsRest decoder
                Partial f    -> decode bs (f (Just bs))
                Done bs' _ i -> (n, Just i) : decode bs' decoder
                  where n = BS.length bs - BS.length bs'

------------------------------------------------------------------------
-- Create the diassembler

opcodeTableSize :: OpcodeTable -> Int
opcodeTableSize (OpcodeTable v) =
  sum (opcodeTableSize <$> v)
opcodeTableSize (OpcodeTableEntry defsWithModRM defsWithoutModRM) =
  length defsWithModRM + length defsWithoutModRM

nextOpcodeSize :: NextOpcodeTable -> Int
nextOpcodeSize v = sum (opcodeTableSize <$> v)

-- | Create an instruction parser from the given udis86 parser.
-- This is currently restricted to x64 base operations.
mkX64Disassembler :: BS.ByteString -> Either String NextOpcodeTable
mkX64Disassembler bs = do
  tbl <- parseOpTable bs
  mOpTbl <- runParserGen $ mkOpcodeTable (filter defSupported tbl)
  case mOpTbl of
    OpcodeTable v -> Right $! v
    OpcodeTableEntry{} -> Left "Unexpected OpcodeTableEntry as a top-level disassemble result"
