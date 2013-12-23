{- |
Module      :  $Header$
Description :  Defines functions for disassembling instructions from optable ast.
Copyright   :  (c) Galois, Inc
Maintainer  :  jhendrix@galois.com

This defines a disassembler based on optable definitions.
-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-} -- for lenses

module Flexdis86.Disassembler
  ( InstructionParser
  , mkX64Parser
  , TableRef
  , parseInstruction
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Int
import           Data.List (subsequences, permutations)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Word
import           Numeric (showHex)

import           Flexdis86.ByteReader
import           Flexdis86.InstructionSet
import           Flexdis86.OpTable

-- | Prefixes for an instruction.
data Prefixes = Prefixes { _prLockPrefix :: LockPrefix
                         , _prSP  :: SegmentPrefix
                         , _prREX :: REX
                         , _prASO :: Bool
                         , _prOSO :: Bool
                         }
                deriving (Show)
                
-- | REX value for 64-bit mode.
newtype REX = REX { unREX :: Word8 }
  deriving (Eq)

instance Show REX where
  showsPrec _ (REX rex) = showHex rex

-- | Includes segment prefix and branch override hints.
newtype SegmentPrefix = SegmentPrefix Word8
  deriving (Show)

$(makeLenses ''Prefixes)

------------------------------------------------------------------------
-- SegmentPrefix

no_seg_prefix :: SegmentPrefix
no_seg_prefix = SegmentPrefix 0

setDefault :: SegmentPrefix -> Segment -> Segment
setDefault (SegmentPrefix 0) s = s
setDefault (SegmentPrefix 0x26) _ = es
setDefault (SegmentPrefix 0x2e) _ = cs
setDefault (SegmentPrefix 0x36) _ = ss
setDefault (SegmentPrefix 0x3e) _ = ds
setDefault (SegmentPrefix 0x64) _ = fs
setDefault (SegmentPrefix 0x65) _ = gs
setDefault (SegmentPrefix w) _ = error $ "Unexpected segment prefix: " ++ showHex w ""


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
     -- | A register or memory location.
     -- As a register has size X-Size, and as memory has size X-Size.
     -- Stored in ModRM.rm
   | MXRX OperandSize OperandSize -- FIXME, should prob. be SizeConstraint

     -- | An MMX register or 64bit memory operand.
     -- Stored in ModRM.rm.
   | RM_MMX
     -- | An MMX register from ModRM.rm
   | RG_MMX_rm

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
    -- Memory value pointing to 64-bit integer stored in ModRM.rm
    -- (ModRM.mod must not equal 3).
  , (,) "Mq"  $ M_X Size64
  , (,) "Md"  $ M_X Size32 
  , (,) "S"    $ RG_S

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

-- | Create an instruction parser from the given udis86 parser.
-- This is currently restricted to x64 base operations.
mkX64Parser :: BS.ByteString -> Either String InstructionParser
mkX64Parser bs = mkParser . filter ifilter <$> parseOpTable bs
  where -- Filter out unsupported operations
        ifilter d = d^.reqAddrSize /= Just Size16
                 && (d^.defCPUReq `elem` [Base, SSE, SSE2, SSE3, SSE4_1, SSE4_2])
                 && x64Compatible d
        mkParser defs = fst $ runParserGen $ parseTable defs

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

-- | Indicates if 64-bit operand size should be used.
rex_w :: REX -> Bool
rex_w r = unREX r `testBit` 3

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
reg8 rex w | rex == no_rex = if w < 4 then low_reg8 w else high_reg8 (w-4)
           | otherwise     = low_reg8 w

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

type AddrOpts = Segment -> Maybe Word8 -> Maybe (Int,Word8) -> Int32 -> AddrRef

memRef_32 :: AddrOpts
memRef_32 s b si o = Addr_32 s (reg32 <$> b) (over _2 reg32 <$> si) o

memRef_64 :: AddrOpts
memRef_64 s b si o = Addr_64 s (reg64 <$> b) (over _2 reg64 <$> si) o

rsp_idx :: Word8
rsp_idx = 4

rbp_idx :: Word8
rbp_idx = 5

sib_si :: (Word8 -> r) -> SIB -> Maybe (Int,r) 
sib_si index_reg sib | idx == rsp_idx = Nothing
                     | otherwise      = Just (2^sib_scale sib, index_reg idx)
  where idx = sib_index sib


-- | A TableRef describes a table of parsers to read based on the bytes.
data TableRef a = TableRef
       { -- | Name of table for pretty printing purposes.
         trName :: !String
       , -- | 256-element vector containing parsers after next byte is read.
         trVec :: !(V.Vector (IParser a))
       }

instance Show (TableRef a) where
  show tr = '#' : trName tr

-- | Return the parser after reading the given byte.
trVal :: TableRef a -> Word8 -> IParser a
trVal tr i = trVec tr `regIdx` i

-- | List of tables in parser with a particular type.
type TableRefSeq a = Seq.Seq (TableRef a)

data ParserState 
   = ParserState { _startTables :: TableRefSeq (InstructionInstance) }
  deriving (Show)

startTables :: Simple Lens ParserState (TableRefSeq InstructionInstance)
startTables = lens _startTables (\s v -> s { _startTables = v})

type ParserGen = State ParserState 

runParserGen :: ParserGen a -> (a,ParserState)
runParserGen p = do
  let s0 = ParserState { _startTables = Seq.empty }
   in runState p s0

mkTableRef :: String
           -> Simple Lens ParserState (TableRefSeq a)
           -> V.Vector (IParser a)
           -> ParserGen (TableRef a)
mkTableRef nm seqLens v = do
  l <- Seq.length <$> use seqLens
  let r = TableRef (nm ++ show l) v
  seq r $ do
    seqLens %= (Seq.|> r)
    return r 

startTableRef :: V.Vector (IParser InstructionInstance)
            -> ParserGen  (TableRef InstructionInstance)
startTableRef = mkTableRef "startTable" startTables

type TableFn t    = [Def]             -> ParserGen t
type PfxTableFn t = [(Prefixes, Def)] -> ParserGen t

type InstructionParser = TableRef InstructionInstance

data IParser a where
  OpcodeTable :: TableRef (InstructionInstance)
              -> IParser  (InstructionInstance)
  SkipModRM :: Prefixes
            -> SizeConstraint
            -> String
            -> [OperandType]
            -> IParser (InstructionInstance)
  ReadModRM :: RegTable (ModTable RMTable)
            -> IParser (InstructionInstance)

deriving instance Show (IParser a)

type OpcodeTable = IParser (InstructionInstance)

regIdx :: V.Vector a -> Word8 -> a
regIdx v i = v V.! fromIntegral i

-- | Returns true if this definition supports the given operand size constaint.
matchRequiredOpSize :: SizeConstraint -> Def -> Bool
matchRequiredOpSize sc d = maybe True (==sc) (d^.reqOpSize)

-- | Returns true if this instruction depends on modrm byte.
expectsModRM :: Def -> Bool
expectsModRM d
  =  isJust (d^.requiredMod)
  || isJust (d^.requiredReg)
  || isJust (d^.requiredRM)
  || any modRMOperand (d^.defOperands)

-- | Returns true if operand has a modRMOperand.
modRMOperand :: String -> Bool
modRMOperand nm =
  case lookupOperandType "" nm of
    OpType sc _ ->
      case sc of
        ModRM_rm        -> True
        ModRM_rm_mod3   -> True
        ModRM_reg       -> True
        Opcode_reg _    -> False
        Reg_fixed _     -> False
        ImmediateSource -> False
        OffsetSource    -> False
        JumpImmediate   -> False
    RG_C   -> True
    RG_dbg -> True
    RG_S   -> True
    RG_MMX_reg -> True
    RG_XMM_reg -> True
    RG_XMM_rm -> True    
    RM_XMM -> True
    SEG _ -> False
    M_FP  -> True
    M     -> True
    M_X _ -> True
    MXRX _ _ -> True
    RM_MMX    -> True
    RG_MMX_rm -> True
    IM_1  -> False
    IM_SB -> False
    IM_SZ -> False

partitionBy :: Show d => [([Word8],d)] -> V.Vector [([Word8],d)]
partitionBy l = V.create $ do
  mv <- VM.replicate 256 []
  forM_ l (go mv) 
  return mv
  where
    go _ ([], d) = error $ "internal: empty bytes in partitionBy at def " ++ show d
    go mv (w:wl,d) = do el <- VM.read mv (fromIntegral w)
                        VM.write mv (fromIntegral w) ((wl,d):el)    


-- | The rules for this are a little convoluted ...
prefixOperandSizeConstraint :: Prefixes -> Def -> SizeConstraint
prefixOperandSizeConstraint pfx d
  -- if the instruction defaults to 64 bit or REX.W is set, then we get 64 bits
  | Just Default64 <- d^.defMode,
    pfx^.prOSO == False = Size64
  | rex_w (pfx^.prREX)  = Size64
  | pfx^.prOSO          = Size16
  | otherwise           = Size32

-- | Some instructions have multiple interpretations depending on the
-- size of the operands.  This is represented by multiple instructions
-- with the same opcode distinguished by the /o= and /a= annotations.
-- This function removes those definitions which don't match the given
-- prefix.
validPrefix :: (Prefixes, Def) -> Bool
validPrefix (pfx, d) = matchRequiredOpSize (prefixOperandSizeConstraint pfx d) d
                       && matchReqAddr (if pfx^.prASO then Size32 else Size64) d
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

-- FIXME: we could probably share more here, rather than recalculating for each instr.
allPrefixedOpcodes :: Def -> [([Word8], (Prefixes, Def))]
allPrefixedOpcodes def = filter (validPrefix . snd) $ map (mkBytes . unzip) rexs
  where
    mkBytes (bytes, funs) = (bytes ++ def^.defOpcodes, (appList funs defaultPrefix, def))
    appList :: [a -> a] -> a -> a
    appList = foldl (.) id
    -- all allowed prefixes from REP, REPZ, ASO, OSO
    simplePfxs :: [ [(Word8, Prefixes -> Prefixes)] ]
    simplePfxs = subsequences (simplePrefixes allowed) 
    -- all possible permutations of the allowed prefixes
    -- FIXME: check that the required prefix doesn't occur in the allowed prefixes
    segs   :: [ [(Word8, Prefixes -> Prefixes)] ]
    segs   = concat 
             $ map permutations -- get all permutations
             $ map (reqPfx ++) -- add required prefix to every allowed prefix
             $ simplePfxs ++ [ v : vs | v <- segPrefixes allowed, vs <- simplePfxs ]
    -- above with REX
    rexs   :: [ [(Word8, Prefixes -> Prefixes)] ]
    rexs   = segs ++ [ seg ++ [v] | seg <- segs, v <- rexPrefixes allowed ]

    allowed = def^.defPrefix
    reqPfx = case def^.requiredPrefix of
               Nothing -> []
               Just b  -> [(b, id)] -- don't do anything, but byte must occur.
    defaultPrefix = Prefixes { _prLockPrefix = NoLockPrefix
                             , _prSP  = no_seg_prefix
                             , _prREX = no_rex
                             , _prASO = False
                             , _prOSO = False }

simplePrefixes :: [String] -> [(Word8, Prefixes -> Prefixes)]
simplePrefixes allowed = [ v | (name, v) <- pfxs, name `elem` allowed ]
  where
    pfxs =  [ ("repz", (0xf2, set prLockPrefix RepZPrefix))
            , ("rep",  (0xf3, set prLockPrefix RepPrefix))
            , ("oso",  (0x66, set prOSO True))
            , ("aso",  (0x67, set prASO True)) ]

segPrefixes :: [String] -> [(Word8, Prefixes -> Prefixes)]
segPrefixes allowed
  | "seg" `elem` allowed = [ (x, set prSP (SegmentPrefix x)) | x <- [ 0x26, 0x2e, 0x36, 0x3e, 0x64, 0x65 ] ]
  | otherwise            = []

-- FIXME: we could also decode the REX
rexPrefixes :: [String] -> [(Word8, Prefixes -> Prefixes)]
rexPrefixes allowed
  | null possibleBits = []
  | otherwise         = [ (x, set prREX (REX x))
                        | xs <- subsequences possibleBits
                        , let x = foldl (.|.) rex_instr_pfx xs ]
  where 
    possibleBits  = [ b | (name, b) <- rexPrefixBits, name `elem` allowed ]
    rexPrefixBits = [ ("rexw", rex_w_bit)
                    , ("rexr", rex_r_bit)
                    , ("rexx", rex_x_bit)
                    , ("rexb", rex_b_bit) ]

-- We calculate all allowed prefixes for the instruction in the first 
-- argument.  This simplifies parsing at the cost of extra space.
mkOpcodeTable :: PfxTableFn (RegTable (ModTable RMTable))
              -> TableFn OpcodeTable
mkOpcodeTable f defs = go [] (concat (map allPrefixedOpcodes defs))
  where -- Recursive function that generates opcode table by parsing
        -- opcodes in first element of list.
        go :: -- Opcode bytes parsed so far.
              [Word8] 
              -- Potential opcode definitions with the remaining opcode
              -- bytes each potential definition expects.
           -> [([Word8], (Prefixes, Def))]
           -> ParserGen OpcodeTable
        go seen l
           -- If we have parsed all the opcodes expected by the remaining
           -- definitions.
          | all opcodeDone l = do
              case l of
                _ | all (expectsModRM.snd.snd) l ->
                     ReadModRM <$> f (snd <$> l) 
                [([],(pfx, d))] -> assert (not (expectsModRM d)) $
                    return $ SkipModRM pfx (prefixOperandSizeConstraint pfx d) (d^.defMnemonic) tps
                  where tps = lookupOperandType "" <$> view defOperands d
                _ -> error $ "mkOpcodeTable: ambiguous operators " ++ show (map snd l)
            -- If we still have opcodes to parse, check that all definitions
            -- expect at least one more opcode, and generate table for next
            -- opcode match.
          | otherwise = assert (all (not.opcodeDone) l) $ do
            let v = partitionBy l
                g i = go (fromIntegral i:seen) (v V.! i)
            fmap OpcodeTable $ startTableRef =<< V.generateM 256 g
        -- Return whether opcode parsing is done.
        opcodeDone :: ([Word8], a) -> Bool
        opcodeDone (remaining,_) = null remaining

data ModTable a
     -- | @ModTable memTable regTable@
   = ModTable !a !a
   | ModUnchecked !a
  deriving (Show)

requireModCheck :: Def -> Bool
requireModCheck d = isJust (d^.requiredMod)

mkModTable :: PfxTableFn a -> PfxTableFn (ModTable a)
mkModTable f pfxdefs
  | any (requireModCheck . snd) pfxdefs = do
    let memDef d =
          case d^.requiredMod of
            Just OnlyReg -> False
            _ -> all modRMMemOperand (d^.defOperands)
        modRMMemOperand nm =
          case lookupOperandType "" nm of
            OpType sc _ ->
              case sc of
                ModRM_rm -> True
                _ -> False
            M_FP    -> True
            M       -> True
            M_X _   -> True
            MXRX _ _ -> True
            RM_MMX  -> True
            _       -> False
    let regDef d =
          case d^.requiredMod of
            Just OnlyMem -> False
            _ -> all regOperand (d^.defOperands)
        regOperand nm =
          case lookupOperandType "" nm of
            OpType sc _ ->
              case sc of
                ModRM_rm -> True
                ModRM_rm_mod3 -> True
                _ -> False
            MXRX _ _  -> True            
            RM_MMX    -> True
            RG_MMX_rm -> True -- FIXME: no MMX_reg?
            RG_XMM_reg -> True 
            RG_XMM_rm -> True             
            RM_XMM    -> True             
            _         -> False
    ModTable <$> f (filter (memDef . snd) pfxdefs)
             <*> f (filter (regDef . snd) pfxdefs)
  | otherwise = ModUnchecked <$> f pfxdefs

data RegTable a
   = RegTable (V.Vector a)
   | RegUnchecked !a
  deriving (Show)

mkRegTable :: (Def -> Maybe Fin8)
           -> PfxTableFn a
           -> PfxTableFn (RegTable a)
mkRegTable rfn f pfxdefs
  | any (isJust . rfn . snd) pfxdefs
  = let p i = maybe True (\r -> unFin8 r == i)
        eltFn i = f $ filter (p i . rfn . snd) pfxdefs
     in RegTable <$> V.generateM 8 (eltFn.fromIntegral)
  | otherwise = RegUnchecked <$> f pfxdefs

type RMTable = RegTable ReadTable

-- | Return parsed instruction.
data ReadTable
   = ReadTable Prefixes SizeConstraint String [OperandType]
   | NoParse
  deriving (Show)

-- | Create a vector of entries by matching an opcode table.
asOpcodeTable :: IParser  (InstructionInstance)
              -> InstructionParser
asOpcodeTable (OpcodeTable v) = v
asOpcodeTable _ = error "asOpcodeTable expected OpcodeTable"

parseTable :: [Def] -> ParserGen InstructionParser
parseTable = fmap asOpcodeTable . makeTables finished
  --TODO: Handle rep and repz prefix filters.
    where
      makeTables = mkOpcodeTable 
                   . mkRegTable (view requiredReg) 
                   . mkModTable 
                   . mkRegTable (view requiredRM) 

      finished []         = return NoParse
      finished [(pfx, d)] = let nm = d^.defMnemonic
                                tps = lookupOperandType "" <$> view defOperands d
                            in
                             return $ ReadTable pfx (prefixOperandSizeConstraint pfx d) nm tps
      finished pfxdefs = fail $ "parseTable ambiguous" ++ show (map snd pfxdefs)
    
------------------------------------------------------------------------
-- Parsing

-- | Read a ModRM byte.
readModRM :: ByteReader m => m ModRM
readModRM = ModRM <$> readByte

-- | Read a SIB byte.
readSIB :: ByteReader m => m SIB
readSIB = SIB <$> readByte

-- | Read 32bit value
read_disp32 :: ByteReader m => m Int32
read_disp32 = readSDWord

-- | Read an 8bit value and sign extend to 32-bits.
read_disp8 :: ByteReader m => m Int32
read_disp8 = fromIntegral <$> readSByte

-- | Parse 
-- | Parse instruction using byte reader.
parseInstruction :: ByteReader m
                 => InstructionParser
                 -> m InstructionInstance
parseInstruction tr0 = do 
  b <- readByte
  case tr0 `trVal` b of
    OpcodeTable tr -> parseInstruction tr
    SkipModRM pfx osz nm tps ->
      II (pfx^.prLockPrefix) nm <$> traverse (parseValue pfx osz Nothing) tps
    ReadModRM t -> flip parseRegTable t =<< readModRM
    
parseGenRegTable :: ByteReader m
              => (ModRM -> a -> m InstructionInstance)
              -> ModRM
              -> RegTable a
              -> m InstructionInstance
parseGenRegTable f modRM (RegTable v) = f modRM mtable
  where mtable = v `regIdx` modRM_reg modRM
parseGenRegTable f modRM (RegUnchecked m) = f modRM m

parseRegTable :: ByteReader m
              => ModRM
              -> RegTable (ModTable RMTable)
              -> m InstructionInstance
parseRegTable = parseGenRegTable parseModTable

parseModTable :: ByteReader m
              => ModRM
              -> ModTable RMTable
              -> m InstructionInstance
parseModTable modRM (ModTable x y) = parseRMTable modRM z
  where z | modRM_mod modRM == 3 = y
          | otherwise = x
parseModTable modRM (ModUnchecked x) = parseRMTable modRM x


parseRMTable :: ByteReader m
             => ModRM
             -> RMTable
             -> m InstructionInstance
parseRMTable = parseGenRegTable parseReadTable

parseReadTable :: ByteReader m
               => ModRM
               -> ReadTable
               -> m InstructionInstance
parseReadTable _ NoParse = fail "Invalid instruction."
parseReadTable modRM (ReadTable pfx osz nm tps) =
  II (pfx^.prLockPrefix) nm <$> traverse (parseValue pfx osz (Just modRM)) tps

-- | Returns the size of a function.
sizeFn :: SizeConstraint -> OperandSize -> SizeConstraint
sizeFn _ BSize = error "internal: sizeFn given BSize"
sizeFn _ WSize = Size16
sizeFn _ DSize = Size32
sizeFn _ QSize = Size64
sizeFn osz VSize = osz
sizeFn Size64 YSize = Size64
sizeFn _      YSize = Size32
sizeFn Size16 ZSize = Size16
sizeFn _      ZSize = Size32
sizeFn _    RDQSize = Size64

regSizeFn :: SizeConstraint -> REX -> OperandSize -> Word8 -> Value
regSizeFn _ rex BSize = ByteReg . reg8 rex
regSizeFn osz _ sz =
  case sizeFn osz sz of
    Size16 -> WordReg  . reg16
    Size32 -> DWordReg . reg32
    Size64 -> QWordReg . reg64

memSizeFn :: SizeConstraint -> OperandSize -> AddrRef -> Value
memSizeFn _ BSize = Mem8
memSizeFn osz sz =
  case sizeFn osz sz of
    Size16 -> Mem16
    Size32 -> Mem32
    Size64 -> Mem64

parseValue :: ByteReader m
           => Prefixes
           -> SizeConstraint -- ^ Operand size
           -> Maybe ModRM
           -> OperandType
           -> m Value
parseValue p osz mmrm tp = do 
  let sp  = p^.prSP
      rex = p^.prREX
      aso = p^.prASO
      msg = "internal: parseValue missing modRM with operand type: " ++ show tp
      modRM = fromMaybe (error msg) mmrm -- laziness is used here and below, this is not used for e.g. ImmediateSource

  let reg = modRM_reg modRM
  let reg_with_rex :: Word8
      reg_with_rex = rex_r rex .|. reg
      addr :: ByteReader m => m AddrRef
      addr = case modRM_mod modRM of
               0 -> readNoOffset aso p modRM
               1 -> readWithOffset read_disp8  aso p modRM
               2 -> readWithOffset read_disp32 aso p modRM 
               _ -> fail $ "internal: parseValue given modRM to register with operand type: " ++ show tp
      rm_reg :: Word8
      rm_reg = rex_b rex .|. modRM_rm modRM

  case tp of
    OpType ModRM_rm sz
      | modRM_mod modRM == 3 -> pure $ regSizeFn osz rex sz rm_reg
      | otherwise            -> memSizeFn osz sz <$> addr
    OpType ModRM_rm_mod3 sz
      | modRM_mod modRM == 3 -> pure $ regSizeFn osz rex sz rm_reg
      | otherwise -> fail $ "Unexpected memory operand in parseValue"
    OpType ModRM_reg sz -> pure $ regSizeFn osz rex sz reg_with_rex
    OpType (Opcode_reg r) sz -> pure $ regSizeFn osz rex sz (rex_b rex .|. r)
    OpType (Reg_fixed r) sz  -> pure $ regSizeFn osz rex sz r
    OpType ImmediateSource BSize -> ByteImm <$> readByte
    OpType ImmediateSource sz ->
      case sizeFn osz sz of
        Size16 ->  WordImm <$> readWord
        Size32 -> DWordImm <$> readDWord
        Size64 -> QWordImm <$> readQWord
    OpType OffsetSource sz
        | BSize <- sz -> Mem8 <$> moffset
        | otherwise -> case sizeFn osz sz of
                         Size16 -> Mem16 <$> moffset
                         Size32 -> Mem32 <$> moffset
                         Size64 -> Mem64 <$> moffset
      where s = sp `setDefault` ds
            moffset | aso =  Offset_32 s <$> readDWord
                    | otherwise = Offset_64 s <$> readQWord
    OpType JumpImmediate BSize -> JumpOffset . fromIntegral <$> readSByte
    OpType JumpImmediate sz -> fmap JumpOffset $
      case sizeFn osz sz of
        Size16 -> fromIntegral <$> readSWord
        Size32 -> fromIntegral <$> readSDWord
        Size64 -> readSQWord

    RG_C   -> return $ ControlReg (controlReg reg_with_rex)
    RG_dbg -> return $ DebugReg   (debugReg   reg_with_rex)
    RG_S   -> SegmentValue <$> segmentRegisterByIndex reg
    RG_MMX_reg -> return $ MMXReg (mmxReg reg)
    RG_XMM_reg -> return $ XMMReg (xmmReg reg)
    RG_XMM_rm  -> return $ XMMReg (xmmReg rm_reg)    
    RM_XMM
      | modRM_mod modRM == 3 ->
        pure $ XMMReg $ xmmReg $ modRM_rm modRM
      | otherwise -> Mem64 <$> addr
    SEG s -> return $ SegmentValue s
    M_FP -> FarPointer <$> addr
    M    ->    VoidMem <$> addr
    M_X sz -> case sz of
                Size16 -> Mem16 <$> addr 
                Size32 -> Mem32 <$> addr 
                Size64 -> Mem64 <$> addr 
    MXRX msz rsz
      | modRM_mod modRM == 3 -> 
        case osz of
          Size16 -> pure $  WordReg $ reg16 rm_reg 
          Size32 -> pure $ DWordReg $ reg32 rm_reg 
          Size64 -> pure $ QWordReg $ reg64 rm_reg 
      | otherwise -> memSizeFn osz msz <$> addr -- FIXME!!
    RM_MMX
      | modRM_mod modRM == 3 ->
        pure $ MMXReg $ mmxReg $ modRM_rm modRM
      | otherwise -> Mem64 <$> addr
    RG_MMX_rm -> assert (modRM_mod modRM == 3) $ do
      pure $ MMXReg $ mmxReg $ modRM_rm modRM
    IM_1 -> pure $ ByteImm 1
    IM_SB -> do
      b <- readSByte
      case osz of
        Size16 -> pure $  WordImm $ fromIntegral b
        Size32 -> pure $ DWordImm $ fromIntegral b
        Size64 -> pure $ QWordImm $ fromIntegral b
    IM_SZ ->
      case osz of
        Size16 ->  WordImm . fromIntegral <$> readSWord
        Size32 -> DWordImm . fromIntegral <$> readDWord
        Size64 -> QWordImm . fromIntegral <$> readDWord

-- FIXME: remove aso, it is in p
readNoOffset :: ByteReader m
             => Bool -- ^ Address size override
             -> Prefixes
             -> ModRM
             -> m AddrRef
readNoOffset aso p modRM = do
  let memRef | aso = memRef_32
             | otherwise = memRef_64
  let rm = modRM_rm modRM
  let rex = p^.prREX
      sp = p^.prSP
  let base_reg  = (rex_b rex .|.)
      index_reg = (rex_x rex .|.)
  if rm == rsp_idx then do
    sib <- readSIB
    let base = sib_base sib
    let si = sib_si index_reg sib
    if base == rbp_idx then do
      memRef (sp `setDefault` ds) Nothing si <$> read_disp32
    else do
      let seg | base == rsp_idx = sp `setDefault` ss
              | otherwise       = sp `setDefault` ds
      return $ memRef seg (Just (base_reg base)) si 0
  else if rm == rbp_idx then do
    let ip_offset | aso = IP_Offset_32
                  | otherwise = IP_Offset_64
    let seg = sp `setDefault` ss
    ip_offset seg <$> read_disp32
  else do
    let seg = sp `setDefault` ds
    return $ memRef seg (Just (base_reg rm)) Nothing 0
    
readWithOffset :: ByteReader m
               => m Int32
               -> Bool -- ^ Address size override
               -> Prefixes
               -> ModRM
               -> m AddrRef
readWithOffset readDisp aso p modRM = do
  let sp = p^.prSP
  let memRef | aso = memRef_32
             | otherwise = memRef_64
  let rex = p^.prREX
  let rm = modRM_rm modRM
  let base_reg  = (rex_b rex .|.)
      index_reg = (rex_x rex .|.)
  (base, si) <-
      if rm == rsp_idx then do
        sib <- readSIB
        return (sib_base sib, sib_si index_reg sib)
      else
        return (rm, Nothing) 
  let seg | base == rsp_idx = ss
          | base == rbp_idx = ss
          | otherwise       = ds
  o <- readDisp
  return $ memRef (sp `setDefault` seg) (Just (base_reg base)) si o
