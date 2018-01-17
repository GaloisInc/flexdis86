{- |
Module      :  $Header$
Copyright   :  (c) Galois, Inc
Maintainer  :  jhendrix@galois.com

This defines a disassembler based on optable definitions.
-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}
module Flexdis86.Disassembler
  ( mkX64Disassembler
  , NextOpcodeTable
  , disassembleInstruction
  , DisassembledAddr(..)
  , tryDisassemble
  , disassembleBuffer
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Data.Binary.Get (Decoder(..), runGetIncremental)
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Int
import           Data.List (subsequences, permutations)
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Word

import           Prelude

import           Flexdis86.ByteReader
import           Flexdis86.InstructionSet
import           Flexdis86.OpTable
import           Flexdis86.Operand
import           Flexdis86.Prefixes
import           Flexdis86.Register
import           Flexdis86.Segment
import           Flexdis86.Sizes

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

type AddrOpts = Segment -> Maybe Word8 -> Maybe (Int,Word8) -> Displacement -> AddrRef

memRef_32 :: AddrOpts
memRef_32 s b si o = Addr_32 s (Reg32 <$> b) (over _2 Reg32 <$> si) o

memRef_64 :: AddrOpts
memRef_64 s b si o = Addr_64 s (Reg64 <$> b) (over _2 Reg64 <$> si) o

rsp_idx :: Word8
rsp_idx = 4

rbp_idx :: Word8
rbp_idx = 5

sib_si :: (Word8 -> Word8) -> SIB -> Maybe (Int,Word8)
sib_si index_reg sib | idx == rsp_idx = Nothing
                     | otherwise      = Just (2^sib_scale sib, idx)
  where idx = index_reg $ sib_index sib


-- | Return parsed instruction.
data ReadTable
   = ReadTable Prefixes SizeConstraint String [OperandType] Def
     -- ^ This indicates we read with the given operand size and size constraint
   | NoParse
  deriving (Show)

data RegTable a
   = RegTable (V.Vector a)
   | RegUnchecked !a
  deriving (Show)

type RMTable = RegTable ReadTable

data ModTable
     -- | @ModTable memTable regTable@
   = ModTable !RMTable !RMTable
   | ModUnchecked !RMTable
  deriving (Show)

------------------------------------------------------------------------
-- OpcodeTable/NextOpcodeTable mutually recursive definitions

data OpcodeTable
   = OpcodeTable NextOpcodeTable
   | SkipModRM Prefixes Def
   | ReadModRM (RegTable ModTable)

-- | A NextOpcodeTable describes a table of parsers to read based on the bytes.
type NextOpcodeTable = V.Vector OpcodeTable

------------------------------------------------------------------------
-- OpcodeTable/NextOpcodeTable instances

deriving instance Show OpcodeTable

type ParserGen = Except String

runParserGen :: ParserGen a -> Either String a
runParserGen p = runExcept p

type PfxTableFn t = [(Prefixes, Def)] -> ParserGen t
-- ^ Given a list of presfixes and a definition this?

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
partitionBy :: [([Word8], (Prefixes, Def))] -> V.Vector [([Word8], (Prefixes, Def))]
partitionBy l = V.create $ do
  mv <- VM.replicate 256 []
  let  go ([], d) = error $ "internal: empty bytes in partitionBy at def " ++ show d
       go (w:wl,d) = do
         el <- VM.read mv (fromIntegral w)
         VM.write mv (fromIntegral w) ((wl,d):el)
  mapM_ go l
  return mv


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

-- | A list of paisr which map prefix bytes corresponding to prefixes to the associated update function.
type PrefixAssignTable = [([Word8], PrefixAssignFun)]

-- Given a list of allowed prefixes
simplePrefixes :: String -> [String] -> PrefixAssignTable
simplePrefixes mnem allowed
  | "rep" `elem` allowed && "repz" `elem` allowed = error $
      "Instruction " ++ mnem ++ " should not be allowed to have both rep and repz as prefixes"
  | otherwise = [ v | (name, v) <- pfxs, name `elem` allowed ]
  where
    pfxs =  [ ("lock",  ([0xf0], set prLockPrefix LockPrefix))
            , ("repnz", ([0xf2], set prLockPrefix RepNZPrefix))
            , ("repz",  ([0xf3], set prLockPrefix RepZPrefix))
            , ("rep",   ([0xf3], set prLockPrefix RepPrefix))
            , ("oso",   ([0x66], set prOSO True))
            , ("aso",   ([0x67], set prASO True))
            ]

-- | Table for segment prefixes
segPrefixes :: [String] -> PrefixAssignTable
segPrefixes allowed
  | "seg" `elem` allowed = [ ([x], set prSP (SegmentPrefix x)) | x <- [ 0x26, 0x2e, 0x36, 0x3e, 0x64, 0x65 ] ]
  | otherwise            = []

-- FIXME: we could probably share more here, rather than recalculating for each instr.
allPrefixedOpcodes :: Def -> [([Word8], (Prefixes, Def))]
allPrefixedOpcodes def
    | checkRequiredPrefix = mapMaybe mkBytes rexs
    | otherwise = error $ "Required prefix of " ++ show (def^.defMnemonic) ++ " overlaps with allowed prefixes."
  where
    appList :: [a -> a] -> a -> a
    appList = foldl (.) id
    -- Get list of permitted prefixes
    allowed = def^.defPrefix
    -- Get all subsets of allowed segmented prefixes
    segPfxs :: PrefixAssignTable
    segPfxs = segPrefixes allowed
    -- Get all subsets of allowed prefixes from REP, REPZ, ASO, OSO
    simplePfxs :: PrefixAssignTable
    simplePfxs = simplePrefixes (def^.defMnemonic) allowed

    -- Function to prepend required prefixes
    checkRequiredPrefix ::  Bool
    checkRequiredPrefix =
      case def^.requiredPrefix of
        Nothing -> True
        -- Required prefix doesn't do anything, but must occur
        Just b  -> all (\(v,_) -> v /= [b]) (segPfxs ++ simplePfxs)

    -- Function to prepend required prefixes
    prependRequiredPrefix :: PrefixAssignTable -> PrefixAssignTable
    prependRequiredPrefix pfx =
      case def^.requiredPrefix of
        Nothing -> pfx
        -- Required prefix doesn't do anything, but must occur
        Just b  -> [([b], id)] ++ pfx
    -- all possible permutations of the allowed prefixes
    -- FIXME: check that the required prefix doesn't occur in the allowed prefixes
    segs   :: [PrefixAssignTable]
    segs   = concatMap permutations -- get all permutations
           $ map prependRequiredPrefix -- add required prefix to every allowed prefix
           $ subsequences simplePfxs ++ [ v : vs | v <- segPfxs, vs <- subsequences simplePfxs ]
    -- above with REX or VEX.  Having both is #UD.
    rexs   :: [PrefixAssignTable]
    rexs
      | null (def ^. vexPrefixes) =
          segs ++ [ seg ++ [v] | seg <- segs, v <- rexPrefixes allowed ]
      | otherwise = [ seg ++ [v] | seg <- segs, v <- mkVexPrefixes def ]

    -- Create the pair containing the byte representation the prefixes and the definition.
    mkBytes :: PrefixAssignTable -> Maybe ([Word8], (Prefixes, Def))
    mkBytes tbl
          | validPrefix pfx def = Just (byte_rep, (pfx, def))
          | otherwise = Nothing
      where (prefix_bytes, funs) = unzip tbl
            -- Get complete binary representation of instruction
            byte_rep = concat prefix_bytes ++ def^.defOpcodes
            -- Get prefix value
            pfx = appList funs defaultPrefix

    defaultPrefix = Prefixes { _prLockPrefix = NoLockPrefix
                             , _prSP  = no_seg_prefix
                             , _prREX = no_rex
                             , _prVEX = Nothing
                             , _prASO = False
                             , _prOSO = False
                             }

-- FIXME: we could also decode the REX
rexPrefixes :: [String] -> PrefixAssignTable
rexPrefixes allowed
  | null possibleBits = []
  | otherwise         = [ ([x], set prREX (REX x))
                        | xs <- subsequences possibleBits
                        , let x = foldl (.|.) rex_instr_pfx xs ]
  where
    possibleBits  = [ b | (name, b) <- rexPrefixBits, name `elem` allowed ]
    rexPrefixBits = [ ("rexw", rex_w_bit)
                    , ("rexr", rex_r_bit)
                    , ("rexx", rex_x_bit)
                    , ("rexb", rex_b_bit) ]


mkVexPrefixes :: Def -> PrefixAssignTable
mkVexPrefixes def = map cvt (def ^. vexPrefixes)
  where
  cvt pref =
    case pref of
      [ _, b ]      -> (pref, set prVEX (Just (VEX2 b)))
      [ _, b1, b2 ] -> (pref, set prVEX (Just (VEX3 b1 b2)))
      _             -> error "vexPrefixes: unexpected byte sequence"

-- We calculate all allowed prefixes for the instruction in the first
-- argument.  This simplifies parsing at the cost of extra space.
mkOpcodeTable ::  [Def] -> ParserGen OpcodeTable
mkOpcodeTable defs = go [] (concatMap allPrefixedOpcodes defs)
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
                _ | all (expectsModRM.snd.snd) l -> do
                     fmap ReadModRM $ checkRequiredReg (snd <$> l)
                [([],(pfx, d))] -> assert (not (expectsModRM d)) $
                    return $ SkipModRM pfx d
                _ -> error $ "mkOpcodeTable: ambiguous operators " ++ show l
            -- If we still have opcodes to parse, check that all definitions
            -- expect at least one more opcode, and generate table for next
            -- opcode match.
          | otherwise = assert (all (not.opcodeDone) l) $ do
            let v = partitionBy l
                g i = go (fromIntegral i:seen) (v V.! i)
            OpcodeTable <$> V.generateM 256 g
        -- Return whether opcode parsing is done.
        opcodeDone :: ([Word8], a) -> Bool
        opcodeDone (remaining,_) = null remaining

requireModCheck :: Def -> Bool
requireModCheck d = isJust (d^.requiredMod)

mkModTable :: [(Prefixes, Def)]
           -> ParserGen ModTable
mkModTable pfxdefs
  | any (requireModCheck . snd) pfxdefs = do
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
    ModTable <$> checkRequiredRM (filter (memDef . snd) pfxdefs)
             <*> checkRequiredRM (filter (regDef . snd) pfxdefs)
  | otherwise =
    ModUnchecked <$> checkRequiredRM  pfxdefs

checkRequiredReg :: PfxTableFn (RegTable ModTable)
checkRequiredReg pfxdefs
  | any (\(_,d) -> isJust (d^.requiredReg)) pfxdefs =
    let p i (_,d) = equalsOptConstraint i (d^.requiredReg)
        eltFn i = mkModTable $ filter (p i) pfxdefs
     in RegTable <$> mkFin8Vector eltFn
  | otherwise = RegUnchecked <$> mkModTable pfxdefs

-- | Make a vector with length 8 from a function over fin8.
mkFin8Vector :: Monad m => (Fin8 -> m v) -> m (V.Vector v)
mkFin8Vector f = V.generateM 8 $ g
  where g i = f j
          where Just j = asFin8 (fromIntegral i)

-- | Return true if value matches an optional constraint (where 'Nothing' denotes any)
-- and 'Just v' denotes a singleton v.
equalsOptConstraint :: Eq a => a -> Maybe a -> Bool
equalsOptConstraint _ Nothing = True
equalsOptConstraint i (Just c) = i == c

-- | Return true if the definition matches the Fin8 constraint
matchRMConstraint :: Fin8 -> (Prefixes,Def)  -> Bool
matchRMConstraint i (_,d) = i `equalsOptConstraint` (d^.requiredRM)


-- | Check required RM if needed, then forward to final table.
checkRequiredRM :: PfxTableFn (RegTable ReadTable)
checkRequiredRM pfxdefs
    -- Split on the required RM value if any of the definitions depend on it
  | any (\(_,d) -> isJust (d^.requiredRM)) pfxdefs =
    let eltFn i = mkFinalTable $ filter (i `matchRMConstraint`) pfxdefs
     in RegTable <$> mkFin8Vector eltFn
  | otherwise = RegUnchecked <$> mkFinalTable pfxdefs

mkFinalTable :: PfxTableFn ReadTable
mkFinalTable []         = return NoParse
mkFinalTable [(pfx, d)] =
  let nm = d^.defMnemonic
      tps = view defOperands d
   in return $ ReadTable pfx (prefixOperandSizeConstraint pfx d) nm tps d
mkFinalTable pfxdefs = throwError $ unlines
                          ("parseTable ambiguous" : map show pfxdefs)

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
read_disp32 = Disp32 <$> readSDWord

-- | Read an 8bit value and sign extend to 32-bits.
read_disp8 :: ByteReader m => m Displacement
read_disp8 = Disp8 <$> readSByte

parseGenRegTable :: (ModRM -> a -> m InstructionInstance)
                 -> (ModRM -> Word8)
                 -> ModRM
                 -> RegTable a
                 -> m InstructionInstance
parseGenRegTable f g modRM (RegTable v) = f modRM mtable
  where mtable = v V.! fromIntegral (g modRM)
parseGenRegTable f _g modRM (RegUnchecked m) = f modRM m

parseReadTable :: ByteReader m
               => ModRM
               -> ReadTable
               -> m InstructionInstance
parseReadTable _ NoParse = invalidInstruction
parseReadTable modRM (ReadTable pfx osz nm tps df) =
  finish <$> traverse (parseValue pfx osz (Just modRM)) tps
  where finish args = II { iiLockPrefix = pfx^.prLockPrefix
                         , iiAddrSize = prAddrSize pfx
                         , iiOp = nm
                         , iiArgs = zipWith (,) args (view defOperands df)
                         , iiPrefixes = pfx
                         , iiRequiredPrefix = view requiredPrefix df
                         , iiOpcode = view defOpcodes df
                         , iiRequiredMod = view requiredMod df
                         , iiRequiredReg = view requiredReg df
                         , iiRequiredRM = view requiredRM df
                         }

parseRMTable :: ByteReader m
             => ModRM
             -> RMTable
             -> m InstructionInstance
parseRMTable = parseGenRegTable parseReadTable modRM_rm

parseModTable :: ByteReader m
              => ModRM
              -> ModTable
              -> m InstructionInstance
parseModTable modRM (ModTable x y) = parseRMTable modRM z
  where z | modRM_mod modRM == 3 = y
          | otherwise = x
parseModTable modRM (ModUnchecked x) = parseRMTable modRM x

parseRegTable :: ByteReader m
              => ModRM
              -> RegTable ModTable
              -> m InstructionInstance
parseRegTable = parseGenRegTable parseModTable modRM_reg

-- | Parse instruction using byte reader.
disassembleInstruction :: ByteReader m
                       => NextOpcodeTable
                       -> m InstructionInstance
disassembleInstruction tr0 = do
  b <- readByte

  case tr0 V.! fromIntegral b of
    OpcodeTable tr -> disassembleInstruction tr
    SkipModRM pfx df ->
        finish <$> traverse (parseValue pfx (prefixOperandSizeConstraint pfx df) Nothing) (df^.defOperands)
      where finish args =
              II { iiLockPrefix = pfx^.prLockPrefix
                 , iiAddrSize = prAddrSize pfx
                 , iiOp   = df^.defMnemonic
                 , iiArgs = zipWith (,) args (view defOperands df)
                 , iiPrefixes = pfx
                 , iiRequiredPrefix = view requiredPrefix df
                 , iiOpcode = view defOpcodes df
                 , iiRequiredMod = view requiredMod df
                 , iiRequiredReg = view requiredReg df
                 , iiRequiredRM = view requiredRM df
                 }
    ReadModRM t -> flip parseRegTable t =<< readModRM

-- | Returns the size of a function.
sizeFn :: SizeConstraint -> OperandSize -> SizeConstraint
sizeFn _ BSize = error "internal: sizeFn given BSize"
sizeFn _ WSize = Size16
sizeFn _ DSize = Size32
sizeFn _ QSize = Size64
sizeFn osz VSize = osz
sizeFn _      OSize = Size128
sizeFn _      QQSize = Size256
sizeFn Size64 YSize = Size64
sizeFn _      YSize = Size32
sizeFn Size16 ZSize = Size16
sizeFn _      ZSize = Size32
sizeFn _    RDQSize = Size64

regSizeFn :: SizeConstraint -> REX -> OperandSize -> Word8 -> Value
regSizeFn _ rex BSize = ByteReg . reg8 rex
regSizeFn osz _ sz =
  case sizeFn osz sz of
    Size16  -> WordReg  . Reg16
    Size32  -> DWordReg . Reg32
    Size64  -> QWordReg . Reg64
    Size128 -> error "Unexpected register size function: Size128"
    Size256 -> error "Unexpected register size function: Size256"

memSizeFn :: SizeConstraint -> OperandSize -> AddrRef -> Value
memSizeFn _ BSize = Mem8
memSizeFn osz sz =
  case sizeFn osz sz of
    Size16 -> Mem16
    Size32 -> Mem32
    Size64 -> Mem64
    Size128 -> Mem128
    Size256 -> Mem256


parseValue :: ByteReader m
           => Prefixes
           -> SizeConstraint -- ^ Operand size
           -> Maybe ModRM
           -> OperandType
           -> m Value
parseValue p osz mmrm tp = do
  let sp  = p^.prSP
      rex = case p ^. prVEX of
              Just vex -> vex ^. vexRex
              _        -> p^.prREX
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
    AbsoluteAddr -> error $ "Absolute addr not yet supported."
    OpType ModRM_rm sz
      | modRM_mod modRM == 3 -> pure $ regSizeFn osz rex sz rm_reg
      | otherwise            -> memSizeFn osz sz <$> addr
    OpType ModRM_rm_mod3 sz
      | modRM_mod modRM == 3 -> pure $ regSizeFn osz rex sz rm_reg
      | otherwise -> fail $ "Unexpected memory operand in parseValue"
    OpType ModRM_reg sz -> pure $ regSizeFn osz rex sz reg_with_rex
    OpType (Opcode_reg r) sz -> pure $ regSizeFn osz rex sz (rex_b rex .|. r)
    OpType (Reg_fixed r) sz  -> pure $ regSizeFn osz rex sz r
    OpType ImmediateSource BSize -> ByteImm <$> readSByte
    OpType ImmediateSource sz ->
      case sizeFn osz sz of
        Size16 ->  WordImm <$> readSWord
        Size32 -> DWordImm <$> readSDWord
        Size64 -> QWordImm <$> readSQWord
        Size128 -> error "128-bit immediates are not supported."
        Size256 -> error "256-bit immediates are not supported."
    OpType OffsetSource sz
        | BSize <- sz -> Mem8 <$> moffset
        | otherwise -> case sizeFn osz sz of
                         Size16 -> Mem16 <$> moffset
                         Size32 -> Mem32 <$> moffset
                         Size64 -> Mem64 <$> moffset
                         Size128 -> error "128-bit offsets are not supported."
                         Size256 -> error "256-bit offsets are not supported."
      where s = sp `setDefault` DS
            moffset | aso =  Offset_32 s <$> readDWord
                    | otherwise = Offset_64 s <$> readQWord
    OpType JumpImmediate BSize -> JumpOffset BSize . fromIntegral <$> readSByte
    OpType JumpImmediate sz -> fmap (JumpOffset sz) $
      case sizeFn osz sz of
        Size16  -> fromIntegral <$> readSWord
        Size32  -> fromIntegral <$> readSDWord
        Size64  -> readSQWord
        Size128 -> error "128-bit jump immediates are not supported."
        Size256 -> error "256-bit jump immediates are not supported."

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
      pure $ memSizeFn Size64 sz a
    IM_1 -> pure $ ByteImm 1
    IM_SB -> ByteImm <$> readSByte
    IM_SZ ->
      -- This reads 16-bits if operand size is 16bits and 32-bits otherwise.
      case osz of
        Size16  ->  WordImm <$> readSWord
        Size32  -> DWordImm <$> readSDWord
        Size64  -> DWordImm <$> readSDWord
        Size128 -> error $ "128-bit immediates are not supported."
        Size256 -> error $ "256-bit immediates are not supported."

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
      memRef (sp `setDefault` DS) Nothing si <$> read_disp32
     else do
      let seg | base == rsp_idx = sp `setDefault` SS
              | otherwise       = sp `setDefault` DS
      return $ memRef seg (Just (base_reg base)) si NoDisplacement
  else if rm == rbp_idx then do
    let ip_offset | aso = IP_Offset_32
                  | otherwise = IP_Offset_64
    let seg = sp `setDefault` SS
    ip_offset seg <$> read_disp32
  else do
    let seg = sp `setDefault` DS
    return $ memRef seg (Just (base_reg rm)) Nothing NoDisplacement

readWithOffset :: ByteReader m
               => m Displacement
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
  let seg | base == rsp_idx = SS
          | base == rbp_idx = SS
          | otherwise       = DS
  o <- readDisp
  return $ memRef (sp `setDefault` seg) (Just (base_reg base)) si o

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
        decode bs _ | BS.null bs = []
        decode bs Fail{} = (1, Nothing):decode bs' decoder
          where Just (_,bs') = BS.uncons bs
        decode bs (Partial f) = decode bs (f (Just bs))
        decode bs (Done bs' _ i) = (n, Just i) : decode bs' decoder
          where n = BS.length bs - BS.length bs'

------------------------------------------------------------------------
-- Create the diassembler

-- | Create an instruction parser from the given udis86 parser.
-- This is currently restricted to x64 base operations.
mkX64Disassembler :: BS.ByteString -> Either String NextOpcodeTable
mkX64Disassembler bs = do
  tbl <- parseOpTable bs
  OpcodeTable v <- runParserGen $ mkOpcodeTable (filter defSupported tbl)
  pure v
