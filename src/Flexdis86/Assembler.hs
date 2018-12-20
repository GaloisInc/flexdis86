{- |
Module      : $Header$
Copyright   : (c) Galois, Inc, 2015-2016
Maintainer  : tristan@galois.com

The Flexdis assembler.
-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
module Flexdis86.Assembler
  ( AssemblerContext
  , mkX64Assembler
  , assemblerContext
  , mkInstruction
  , assembleInstruction
  ) where

import           GHC.Stack

import           Control.Applicative
import           Control.Arrow ( second )
import qualified Control.Lens as L
import           Control.Monad ( MonadPlus(..), guard )
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import           Data.Maybe ( fromMaybe, isJust )
import           Data.Monoid
import           Data.Word

import           Prelude

import           Flexdis86.Operand
import           Flexdis86.OpTable
import           Flexdis86.InstructionSet
import           Flexdis86.Prefixes
import           Flexdis86.Register
import           Flexdis86.Segment
import           Flexdis86.Sizes

data AssemblerContext =
  AssemblerContext { acDefs :: M.Map String [Def]
                   }
  deriving (Show)

mkX64Assembler :: B.ByteString -> Either String AssemblerContext
mkX64Assembler bs = (assemblerContext . filter defSupported) <$> parseOpTable bs

-- Warning: some instructions, e.g. @xor <64-bit reg> <64-bit reg>@,
-- have multiple encodings. The order we fold defs here, i.e. 'foldr'
-- vs 'foldl', determines which of several encodings we get.
assemblerContext :: [Def] -> AssemblerContext
assemblerContext = AssemblerContext . foldr addDef M.empty
  where
    -- Add def once under every mnemonic, including synonyms.
    def `addDef` map' = foldr add map' keys
      where
      key `add` map'' = M.alter (Just . (maybe [def] (def:))) key map''
      keys = L.view defMnemonic def : L.view defMnemonicSynonyms def

mkInstruction :: (MonadPlus m)
              => AssemblerContext
              -> String
              -- ^ Mnemonic
              -> [Value]
              -- ^ Arguments
              -> m InstructionInstance
mkInstruction ctx mnemonic args =
   foldr (<|>) empty (map (findEncoding args) defs)
  where
    defs = fromMaybe [] $ M.lookup mnemonic (acDefs ctx)

findEncoding :: (MonadPlus m) => [Value] -> Def -> m InstructionInstance
findEncoding args def = do
  let opTypes = L.view defOperands def
  guard (length args == length opTypes)
  let argTypes = zip args opTypes
  -- Always only consider case where operand size override is false.
  -- NOTE. We could extend this to consider both options.
  let oso = False
  F.forM_ argTypes $ \at -> guard (matchOperandType oso at)
  let rex = mkREX argTypes
  let vex = Nothing -- XXX: implement this
  return $ II { iiLockPrefix = NoLockPrefix
              -- ???: why is this always Size16? Can we do better
              -- based on args or def?
              , iiAddrSize = Size16
              , iiOp = L.view defMnemonic def
              , iiArgs = zip args opTypes
              , iiPrefixes = Prefixes { _prLockPrefix = NoLockPrefix
                                      , _prSP = no_seg_prefix
                                      , _prREX = rex
                                      , _prVEX = vex
                                      , _prASO = False
                                      , _prOSO = oso
                                      }
              , iiRequiredPrefix = L.view requiredPrefix def
              , iiOpcode      = L.view defOpcodes  def
              , iiRequiredMod = L.view requiredMod def
              , iiRequiredReg = L.view requiredReg def
              , iiRequiredRM  = L.view requiredRM  def
              }

-- | The REX prefix modifies instructions to operate over 64 bit operands.
--
-- The format of the byte is:
--
-- > 0100WRXB
--
-- W is 1 if the operands are 64 bit sized.  We set that with a fold
-- over all of the arguments; if any argument is 64 bits, we set W.
--
-- R is an extension to the reg field, so we set that if the reg is
-- a reference to a register requiring the extra bit (r8 or higher).
--
-- X is an extension to the SIB field, and isn't supported yet... FIXME
--
-- B is an extension to the r/m field, so we set that if the r/m
-- refers to r8 or greater.
--
-- FIXME: Knowing what is r/m and what is reg is kind of difficult.
-- What we have here mostly works for now, but it will be more
-- complicated in the limit. See e.g. 'addREXbFlag' for weird "knowing
-- what is rm" case.
mkREX :: [(Value, OperandType)] -> REX
mkREX vos =
  -- If we didn't set any of the variable REX bits -- i.e. we didn't
  -- set any of W, R, X, or B -- then we don't set the REX at all.
  case foldr setFlags rex0 vos of
    rex1 | rex1 == rex0 -> REX 0
         | otherwise -> rex1
  where
    rex0 = REX 0b01000000
    setFlags (arg, ty) = addREXrFlag arg ty .
                         addREXbFlag arg ty .
                         addREXwFlag arg

setREXFlagIf :: L.ASetter t t a Bool -> Reg64 -> t -> t
setREXFlagIf flg (Reg64 rno) rex
  | rno >= 8 = L.set flg True rex
  | otherwise = rex

-- | Set REX.r when the arg is MODRM.reg and and is one of r8 thru
-- r15.
addREXrFlag :: Value -> OperandType -> REX -> REX
addREXrFlag v o rex =
  case (v, o) of
    (QWordReg reg, OpType ModRM_reg _) -> setREXFlagIf rexR reg rex
    _ -> rex

-- | Set REX.b when the arg is MODRM.rm and and is one of r8 thru r15.
addREXbFlag :: Value -> OperandType -> REX -> REX
addREXbFlag v o rex =
  case (v, o) of
    (QWordReg reg, OpType ModRM_rm _)       -> setREXFlagIf rexB reg rex
    -- This case is suspect. Conathan put it here so that the
    -- @movq $0x190000000,%r11@ test would pass, but it's not clear why
    -- this @OpType@ is a MODRM.rm.
    (QWordReg reg, OpType (Opcode_reg _) _) -> setREXFlagIf rexB reg rex
    _ -> rex

addREXwFlag :: Value -> REX -> REX
addREXwFlag v r =
  case v of
    QWordReg {} -> L.set rexW True r
    _ -> r

-- Need to build prefixes based on arg sizes...
--
-- Maybe the checking should occur in a Monad and we can build up any
-- modifiers to the prefixes on the fly?
--
-- ASO and OSO might be painful, since the meaning varies.
--
-- Lock would be easy (and probably optional)
--
-- REX should be pretty easy, since it just encodes extra bits in
-- modr/m

-- | Return True if the given 'Value' can be encoded with the operand
-- type (specified as a String).
--
-- If 'mkInstruction' is failing due to 'matchOperandType' returning
-- false, you can figure out what argument patterns to add by using
-- @:/utils/dump.sh@ to see how flexdis86 encodes the instruction in
-- question. E.g. for @or eax, eax@:
--
-- @
-- $ stack utils/dump.sh "or eax,eax"
-- 09 c0                   or     eax,eax
-- II
--   { iiLockPrefix = NoLockPrefix
--   , iiAddrSize = Size64
--   , iiOp = "or"
--   , iiArgs =
--       [ ( DWordReg eax , OpType ModRM_rm VSize )
--       , ( DWordReg eax , OpType ModRM_reg VSize )
--       ]
--   , iiPrefixes =
--       Prefixes
--         { _prLockPrefix = NoLockPrefix
--         , _prSP = SegmentPrefix { unwrapSegmentPrefix = 0 }
--         , _prREX = 0
--         , _prVEX = Nothing
--         , _prASO = False
--         , _prOSO = False
--         }
--   , iiRequiredPrefix = Nothing
--   , iiOpcode = [ 9 ]
--   , iiRequiredMod = Nothing
--   , iiRequiredReg = Nothing
--   , iiRequiredRM = Nothing
--   }
-- or     eax,eax
-- @
--
-- Which tells us the encodings are
--
-- >   iiArgs = [(DWordReg eax,OpType ModRM_rm VSize),(DWordReg eax,OpType ModRM_reg VSize)]
matchOperandType :: Bool -> (Value, OperandType) -> Bool
matchOperandType oso ops =
  case ops of
    (ByteImm _, OpType ImmediateSource BSize) -> True
    (WordImm _, OpType ImmediateSource WSize) -> True
    (DWordImm _, OpType ImmediateSource DSize) -> True
    (QWordImm _, OpType ImmediateSource QSize) -> True
    -- Note, these two will depend on the operand mode...
--    (WordImm _, OpType ImmediateSource VSize) -> True
    (DWordImm _, OpType ImmediateSource VSize) -> True
    (QWordImm _, OpType ImmediateSource VSize) -> True
--    (WordImm _, OpType ImmediateSource YSize) -> True
    (DWordImm _, OpType ImmediateSource YSize) -> True
    (QWordImm _, OpType ImmediateSource YSize) -> True
    (WordImm _, OpType ImmediateSource ZSize) -> True
    (DWordImm _, OpType ImmediateSource ZSize) -> True
    (QWordImm _, OpType ImmediateSource ZSize) -> True
    (DWordImm _, OpType ImmediateSource RDQSize) -> True
    (QWordImm _, OpType ImmediateSource RDQSize) -> True
    (ByteSignedImm _, IM_SB) -> True
    (WordSignedImm _, IM_SZ) | oso == True -> True
    (DWordSignedImm _, IM_SZ) | oso == False -> True
    (JumpOffset JSize8 _,  OpType JumpImmediate BSize) -> True
    (JumpOffset JSize16 _, OpType JumpImmediate ZSize) | oso == True  -> True
    (JumpOffset JSize32 _, OpType JumpImmediate ZSize) | oso == False -> True
    (DWordReg _, OpType ModRM_rm VSize) -> True
    (QWordReg _, OpType ModRM_rm QSize) -> True
    (QWordReg _, OpType ModRM_rm VSize) -> True
    (QWordReg _, OpType ModRM_rm YSize) -> True
    (QWordReg _, OpType ModRM_rm RDQSize) -> True
    (Mem64 _, OpType ModRM_rm QSize) -> True
    (Mem64 _, OpType ModRM_rm VSize) -> True
    (Mem64 _, OpType ModRM_rm YSize) -> True
    (Mem64 _, OpType ModRM_rm RDQSize) -> True
    (Mem128 _, RM_XMM Nothing) -> True
    (XMMReg _, RG_XMM_reg Nothing) -> True
    (DWordReg _, OpType ModRM_reg VSize) -> True
    (QWordReg _, OpType ModRM_reg QSize) -> True
    (QWordReg _, OpType ModRM_reg VSize) -> True
    (QWordReg _, OpType ModRM_reg YSize) -> True
    (QWordReg _, OpType ModRM_reg RDQSize) -> True
    (QWordReg (Reg64 rno), OpType (Opcode_reg rcode) VSize) -> rno `mod` 8 == rcode
    _ -> False

-- | Create a bytestring builder from an instruction instance.
assembleInstruction :: (HasCallStack, MonadPlus m) => InstructionInstance -> m B.Builder
assembleInstruction ii = do
  return $ mconcat [ prefixBytes
                   , opcode
                   , fromMaybe mempty (encodeModRMDisp ii)
                   , mconcat (map (encodeImmediate rex oso) (iiArgs ii))
                   ]
  where
    rex = L.view prREX pfxs
    oso = L.view prOSO pfxs
    spfx = L.view prSP pfxs
    prefixBytes = mconcat [ if spfx == no_seg_prefix then mempty else B.word8 (unwrapSegmentPrefix spfx)
                          , if L.view prASO pfxs then B.word8 0x67 else mempty
                          , if oso then B.word8 0x66 else mempty
                          , encodeLockPrefix (L.view prLockPrefix pfxs)
                          , encodeRequiredPrefix (iiRequiredPrefix ii)
                          , encodeREXPrefix rex
                          ]
    opcode = B.byteString (B.pack (iiOpcode ii))
    pfxs = iiPrefixes ii

{-

If a REX prefix is set, we are in 64 bit mode.

-}

-- | Construct the ModR/M, SIB, and Displacement bytes
--
-- The arguments all determine these together, so we really need to
-- build a combined byte string.
encodeModRMDisp :: (Alternative m, HasCallStack) => InstructionInstance -> m B.Builder
encodeModRMDisp ii
  | not (hasModRM ii) = empty
  | otherwise = encodeOperandModRM ii req <|> empty
  where
    req = encodeRequiredModRM ii

hasModRM :: InstructionInstance -> Bool
hasModRM ii = or [ isJust (iiRequiredMod ii)
                 , isJust (iiRequiredReg ii)
                 , isJust (iiRequiredRM ii)
                 , isJust (iiRequiredPrefix ii)
                 , any operandTypeRequiresModRM (map snd (iiArgs ii))
                 ]

operandTypeRequiresModRM :: OperandType -> Bool
operandTypeRequiresModRM ot =
  case ot of
    IM_1 -> False
    IM_SB -> False
    IM_SZ -> False
    OpType ImmediateSource _ -> False
    OpType (Opcode_reg _) _ -> False
    OpType (Reg_fixed _) _ -> False
    OpType OffsetSource _ -> False
    _ -> True

isImplicitRegister :: OperandType -> Bool
isImplicitRegister t =
  case t of
    OpType (Reg_fixed _) _ -> True
    _ -> False

-- | Build a ModRM byte based on the operands of the instruction.
encodeOperandModRM :: (Alternative m, HasCallStack) => InstructionInstance -> Word8 -> m B.Builder
encodeOperandModRM ii reqModRM
  | [] <- filter (isNotImmediate . fst) (iiArgs ii)
  , reqModRM /= 0
  = pure $ B.word8 reqModRM
  | otherwise
  = case (rrv_rm rrv, rrv_reg rrv) of
      (Nothing, Nothing) -> empty
      (Nothing, Just _) -> error $
        "encodeOperandModRM: unexpected RmRegValues: " ++ show rrv
      (Just (vrm, _), Nothing) ->
        pure $ withMode vrm $ \mode disp ->
          let rm = encodeValue vrm
          in withSIB mode rm vrm $ \sib ->
               mkModRM ii mode 0 rm <> sib <> disp
      (Just (vrm, _), Just (vreg, _)) ->
        pure $ withMode vrm $ \mode disp ->
          let rm = encodeValue vrm
          in withSIB mode rm vrm $ \sib ->
               mkModRM ii mode (encodeValue vreg) rm <> sib <> disp
  where
    rrv = findRmRegValues (iiArgs ii)

-- | The arguments (if any) that should be encoded into the rm and reg
-- bits of the ModRM byte and the corresponding bits in the REX
-- prefix.
data RmRegValues =
  RmRegValues { rrv_rm :: Maybe (Value, OperandType)
              , rrv_reg :: Maybe (Value, OperandType) }
  deriving (Eq, Show)

-- | Find the arguments that should be encoded as rm and reg.
findRmRegValues :: [(Value, OperandType)] -> RmRegValues
findRmRegValues args =
  case filter (isNotImmediate . fst) args of
    [] -> emptyRm
    [(_, M_Implicit {}), (_, M_Implicit {})] -> emptyRm
    [(_, M_Implicit {})] -> emptyRm
    [(_, M_Implicit {}), (_, ot)]
      | isImplicitRegister ot -> emptyRm
    [(_, ot), (_, M_Implicit {})]
      | isImplicitRegister ot -> emptyRm
    [imp@(_, M_Implicit {}), _] -> error ("Unexpected implicit (1): " ++ show imp)
    [_, imp@(_, M_Implicit {})] -> error ("Unexpected implicit (2): " ++ show imp)
    [arg1] -> RmRegValues { rrv_rm = Just arg1, rrv_reg = Nothing }
    [arg1, arg2] ->
      let (rm, reg) = withRMFirst arg1 arg2
      in RmRegValues { rrv_rm = Just rm, rrv_reg = Just reg }
    _ -> emptyRm
    where
      emptyRm = RmRegValues Nothing Nothing

-- | Re-order the arguments such that the RM operand is the first
-- argument of the callback.
--
-- This makes encoding of the ModR/M byte simpler
--
-- The real concern is that, in the case where the second argument is
-- encoded in R/M, we need to swap the order so that
-- 'encodeOperandModRM' puts it in the right field.  Otherwise, we
-- preserve the original order.
withRMFirst :: (Value, OperandType)
            -> (Value, OperandType)
            -> ((Value, OperandType), (Value, OperandType)) {-^(rm, reg)-}
withRMFirst a1 a2@(_v2, v2ty) =
  case v2ty of
    OpType ModRM_rm _ -> (a2, a1)
    OpType ModRM_rm_mod3 _ -> (a2, a1)
    RG_XMM_rm {} -> (a2, a1)
    RM_XMM {} -> (a2, a1)
    M_FP -> (a2, a1)
    M -> (a2, a1)
    M_X {} -> (a2, a1)
    M_FloatingPoint {} -> (a2, a1)
    MXRX {} -> (a2, a1)
    RM_MMX -> (a2, a1)
    RG_MMX_rm -> (a2, a1)
    _ -> (a1, a2)

hasScaledIndex :: Value -> Bool
hasScaledIndex v
  | Just addrRef <- memRefComponents v =
    case addrRef of
      Addr_32 _ _ (Just _) _ -> True
      Addr_64 _ _ (Just _) _ -> True
      _ -> False
  | otherwise = False

-- | Provide the SIB to the callback, if it is required
withSIB :: Word8 -> Word8 -> Value -> (B.Builder -> a) -> a
withSIB mode rm val k
  | not (hasScaledIndex val) && (not (requiresSIB rm) || mode == directRegister) = k mempty
  | Just addrRef <- memRefComponents val =
    case addrRef of
      Addr_32 seg mbase midx _ ->
        let midx' = fmap (second (unReg64 . reg32_reg)) midx
            mbase' = fmap (unReg64 . reg32_reg) mbase
        in k (mkSIB seg midx' mbase')
      Addr_64 seg mbase midx _ ->
        let midx' = fmap (second unReg64) midx
            mbase' = fmap unReg64 mbase
        in k (mkSIB seg midx' mbase')
      _ -> k mempty
  | otherwise = k mempty

mkSIB :: Segment
         -- ^ The segment
      -> Maybe (Int, Word8)
         -- ^ (optional) (2^Scale, Index)
      -> Maybe Word8
         -- ^ Register base
      -> B.Builder
mkSIB seg mScaleIdx mBase =
  case (mScaleIdx, mBase) of
    (Nothing, Just rno) -> B.word8 ((4 `shiftL` 3) .|. (rno .&. 0x7))
    (Just (scale, ix), Just rno) ->
      B.word8 ((round (logBase 2 (fromIntegral scale) :: Double) `shiftL` 6) .|. ((ix .&. 0x7) `shiftL` 3) .|. (rno .&. 0x7))
    -- With no base, the base part of the SIB is 0x5, according to the table (the [*] column).
    (Just (scale, ix), Nothing) ->
      B.word8 ((round (logBase 2 (fromIntegral scale) :: Double) `shiftL` 6) .|. ((ix .&. 0x7) `shiftL` 3) .|. 0x5)
    (Nothing, Nothing) | seg `elem` [FS, GS] -> B.word8 0x25
    other -> error ("Unexpected inputs to mkSIB: " ++ show other)

requiresSIB :: Word8 -> Bool
requiresSIB = (==0x4)

-- | Compute the mode bits and displacement for a 'Value'.
-- Eventually, this will also compute the SIB.
--
-- The three are all very closely related, so computing them all at
-- once makes the most sense.
withMode :: Value -> (Word8 -> B.Builder -> a) -> a
withMode v k =
  case v of
    ByteReg {} -> k directRegister mempty
    WordReg {} -> k directRegister mempty
    DWordReg {} -> k directRegister mempty
    QWordReg {} -> k directRegister mempty
    MMXReg {} -> k directRegister mempty
    XMMReg {} -> k directRegister mempty
    X87Register {} -> k directRegister mempty

    _ | Just comps <- memRefComponents v ->
        case comps of
          Addr_64 _ (Just _) _ NoDisplacement -> k noDisplacement mempty
          Addr_32 _ (Just _) _ NoDisplacement -> k noDisplacement mempty

          Addr_64 _ Nothing _ (Disp32 (Imm32Concrete d)) -> k noDisplacement (B.int32LE d)
          Addr_32 _ Nothing _ (Disp32 (Imm32Concrete d)) -> k noDisplacement (B.int32LE d)

          Addr_64 _ _ _ (Disp32 (Imm32Concrete d)) -> k disp32 (B.int32LE d)
          Addr_32 _ _ _ (Disp32 (Imm32Concrete d)) -> k disp32 (B.int32LE d)

          Addr_64 _ _ _ (Disp8 d) -> k disp8 (B.int8 d)
          Addr_32 _ _ _ (Disp8 d) -> k disp8 (B.int8 d)

          _ -> error ("mkMode: Unsupported memory ref type " ++ show v)
      | Just comps <- ripRefComponents v ->
        case comps of
          IP_Offset_32 _ (Disp32 (Imm32Concrete d)) -> k noDisplacement (B.int32LE d)
          IP_Offset_64 _ (Disp32 (Imm32Concrete d)) -> k noDisplacement (B.int32LE d)
          _ -> error ("mkMode: Unsupported rip offset type " ++ show v)

      | otherwise -> error ("mkMode: Unsupported mode for " ++ show v)

-- | This is the "direct register" addressing method with the value
-- already shifted appropriately.
directRegister :: Word8
directRegister = 0x3

noDisplacement :: Word8
noDisplacement = 0

disp8 :: Word8
disp8 = 0x1

disp32 :: Word8
disp32 = 0x2

memRefComponents :: Value -> Maybe AddrRef
memRefComponents v =
  case v of
    Mem128 addr@(Addr_64 {}) -> Just addr
    Mem64 addr@(Addr_64 {}) -> Just addr
    Mem32 addr@(Addr_64 {}) -> Just addr
    Mem16 addr@(Addr_64 {}) -> Just addr
    Mem8 addr@(Addr_64 {}) -> Just addr
    VoidMem addr@(Addr_64 {}) -> Just addr
    FPMem32 addr@(Addr_64 {}) -> Just addr
    FPMem64 addr@(Addr_64 {}) -> Just addr
    FPMem80 addr@(Addr_64 {}) -> Just addr

    Mem128 addr@(Addr_32 {}) -> Just addr
    Mem64 addr@(Addr_32 {}) -> Just addr
    Mem32 addr@(Addr_32 {}) -> Just addr
    Mem16 addr@(Addr_32 {}) -> Just addr
    Mem8 addr@(Addr_32 {}) -> Just addr
    VoidMem addr@(Addr_32 {}) -> Just addr
    FPMem32 addr@(Addr_32 {}) -> Just addr
    FPMem64 addr@(Addr_32 {}) -> Just addr
    FPMem80 addr@(Addr_32 {}) -> Just addr

    _ -> Nothing

ripRefComponents :: Value -> Maybe AddrRef
ripRefComponents v =
  case v of
    Mem128 addr@(IP_Offset_64 {}) -> Just addr
    Mem64 addr@(IP_Offset_64 {}) -> Just addr
    Mem32 addr@(IP_Offset_64 {}) -> Just addr
    Mem16 addr@(IP_Offset_64 {}) -> Just addr
    Mem8 addr@(IP_Offset_64 {}) -> Just addr
    VoidMem addr@(IP_Offset_64 {}) -> Just addr
    FPMem32 addr@(IP_Offset_64 {}) -> Just addr
    FPMem64 addr@(IP_Offset_64 {}) -> Just addr
    FPMem80 addr@(IP_Offset_64 {}) -> Just addr

    Mem128 addr@(IP_Offset_32 {}) -> Just addr
    Mem64 addr@(IP_Offset_32 {}) -> Just addr
    Mem32 addr@(IP_Offset_32 {}) -> Just addr
    Mem16 addr@(IP_Offset_32 {}) -> Just addr
    Mem8 addr@(IP_Offset_32 {}) -> Just addr
    VoidMem addr@(IP_Offset_32 {}) -> Just addr
    FPMem32 addr@(IP_Offset_32 {}) -> Just addr
    FPMem64 addr@(IP_Offset_32 {}) -> Just addr
    FPMem80 addr@(IP_Offset_32 {}) -> Just addr

    _ -> Nothing

-- | Encode a value operand as a three bit RM nibble
encodeValue :: HasCallStack => Value -> Word8
encodeValue v =
  case v of
    ByteReg r8 -> 0x7 .&. reg8ToRM r8
    WordReg (Reg16 rno) -> 0x7 .&. rno
    DWordReg (Reg32 rno) -> 0x7 .&. rno
    QWordReg (Reg64 rno) -> 0x7 .&. rno
    X87Register rno -> 0x7 .&. fromIntegral rno
    MMXReg (MMXR rno) -> rno
    XMMReg (XMMR rno) -> rno
    _ | Just comps <- memRefComponents v ->
        case comps of
          -- We just need to mask some bits off of the reg64 numbers
          Addr_64 _ (Just (Reg64 rno)) Nothing _ -> 0x7 .&. rno
          Addr_32 _ (Just (Reg32 rno)) Nothing _ -> rno

          -- A scaled index with no base indicates that we need a SIB
          Addr_64 _ _ (Just _) _ -> 0x4
          Addr_32 _ _ (Just _) _ -> 0x4

          -- There is another type of scaled index that is less
          -- apparent: if the segment is fs or gs, the displacement is
          -- used as a scaled index from that register.
          Addr_64 seg Nothing Nothing _ | seg `elem` [FS, GS] -> 0x04
          Addr_32 seg Nothing Nothing _ | seg `elem` [FS, GS] -> 0x04

          -- If there is no base and no index at all, 0x5 indicates
          -- that ModR/M is followed by a raw displacement.
          Addr_64 _ Nothing Nothing _ -> 0x5
          Addr_32 _ Nothing Nothing _ -> 0x5

          _ -> error ("encodeValue: Unsupported memRef type " ++ show v)
      | Just comps <- ripRefComponents v ->
        case comps of
          IP_Offset_64 {} -> 0x5
          IP_Offset_32 {} -> 0x5
          _ -> error ("encodeValue: Unsupported rip offset " ++ show v)
      | otherwise -> error ("encodeValue: Unknown value type " ++ show v)

-- If there is a displacement, I think we need to return 0b101 here.

isNotImmediate :: Value -> Bool
isNotImmediate val =
  case val of
    ByteImm {} -> False
    WordImm {} -> False
    DWordImm {} -> False
    QWordImm {} -> False
    JumpOffset {} -> False
    ByteSignedImm {} -> False
    WordSignedImm {} -> False
    DWordSignedImm {} -> False
    ControlReg {} -> True
    DebugReg {} -> True
    MMXReg {} -> True
    XMMReg {} -> True
    YMMReg {} -> True
    SegmentValue {} -> True
    X87Register {} -> True
    FarPointer {} -> True
    VoidMem {} -> True
    Mem8 {} -> True
    Mem16 {} -> True
    Mem32 {} -> True
    Mem64 {} -> True
    Mem128 {} -> True
    Mem256 {} -> True
    FPMem32 {} -> True
    FPMem64 {} -> True
    FPMem80 {} -> True
    ByteReg {} -> True
    WordReg {} -> True
    DWordReg {} -> True
    QWordReg {} -> True

-- | We represent the high registers (e.g., ah) as 16+regnum.
--
-- We need to remove the 16 we added (hence the 0xf mask).  Further,
-- x86 represents the high registers starting ah at 4.
reg8ToRM :: Reg8 -> Word8
reg8ToRM (HighReg8 rno) = rno + 4
reg8ToRM (LowReg8 rno) = rno
reg8ToRM _ = error "reg8ToRM encountered unexpected case."

-- | From constituent components, construct the ModRM byte with
-- appropriate shifting.
mkModRM :: InstructionInstance -> Word8 -> Word8 -> Word8 -> B.Builder
mkModRM ii modb regb rmb =
  B.word8 (rm .|. (reg `shiftL` 3) .|. (rmmod `shiftL` 6))
  where
    reg = maybe regb unFin8 (iiRequiredReg ii)
    rm = maybe rmb unFin8 (iiRequiredRM ii)
    rmmod = maybe modb modConstraintVal (iiRequiredMod ii)

-- | Build a ModRM byte from a full set of entirely specified mod/rm
-- bits.
--
-- If there are no arguments to the instruction, we can take a partial
-- set of mod/reg/rm values and compute the byte with zeros for the
-- missing values.
encodeRequiredModRM :: InstructionInstance -> Word8
encodeRequiredModRM ii =
  fromMaybe 0 rmod .|. fromMaybe 0 reg .|. fromMaybe 0 rm
  where
    rmod = fmap ((`shiftL`  6) . modConstraintVal) (iiRequiredMod ii)
    reg = fmap ((`shiftL` 3) . unFin8) (iiRequiredReg ii)
    rm  = fmap unFin8 (iiRequiredRM ii)

modConstraintVal :: ModConstraint -> Word8
modConstraintVal mc =
  case mc of
    OnlyReg -> 3
    OnlyMem -> 0

encodeRequiredPrefix :: Maybe Word8 -> B.Builder
encodeRequiredPrefix = maybe mempty B.word8

encodeLockPrefix :: LockPrefix -> B.Builder
encodeLockPrefix pfx =
  case pfx of
    NoLockPrefix -> mempty
    LockPrefix -> B.word8 0xF0
    RepNZPrefix -> B.word8 0xF2
    RepPrefix -> B.word8 0xF3
    RepZPrefix -> B.word8 0xF3

-- | Right now, a zero REX prefix is ignored and any other value is
-- returned directly.  Not entirely sure if that is correct, but it
-- seems to work okay.
encodeREXPrefix :: REX -> B.Builder
encodeREXPrefix (REX rex) | rex == 0 = mempty
                          | otherwise = B.word8 (setBit rex 6)

encodeImmediate :: REX -> Bool -> (Value, OperandType) -> B.Builder
encodeImmediate rex oso vty =
  case vty of
    (ByteImm imm, ty) -> encodeByteImmediate rex oso imm ty
    (WordImm imm, ty) -> encodeWordImmediate rex oso imm ty
    (DWordImm (Imm32Concrete imm), ty) -> encodeDWordImmediate rex oso (fromIntegral imm) ty
    (DWordImm Imm32SymbolOffset{}, _) -> error "Do not support symbolic immediates."
    (QWordImm imm, ty) -> encodeQWordImmediate rex oso imm ty

    (ByteSignedImm imm, ty)  -> encodeByteImmediate  rex oso (fromIntegral imm) ty
    (WordSignedImm imm, ty)  -> encodeWordImmediate  rex oso (fromIntegral imm) ty
    (DWordSignedImm imm, ty) -> encodeDWordImmediate rex oso (fromIntegral imm) ty

    (Mem32 (Offset_64 _ o), ty) -> encodeQWordImmediate rex oso o ty
    (Mem64 (Offset_64 _ o), ty) -> encodeQWordImmediate rex oso o ty

    (JumpOffset JSize8 (FixedOffset off),  OpType JumpImmediate BSize) -> B.int8 (fromIntegral off)
    (JumpOffset JSize16 (FixedOffset off), OpType JumpImmediate ZSize) -> B.int16LE (fromIntegral off)
    (JumpOffset JSize32 (FixedOffset off), OpType JumpImmediate ZSize) -> B.int32LE (fromIntegral off)
    (JumpOffset _ _, _) -> error $
      "Unhandled jump offset immediate: " ++ show vty ++ "(rex=" ++ show rex ++ ", oso=" ++ show oso ++ ")"
    _ -> mempty

encodeByteImmediate :: REX -> Bool -> Word8 -> OperandType -> B.Builder
encodeByteImmediate rex oso b ty =
  case ty of
    OpType ImmediateSource BSize -> B.word8 b
    IM_1 -> mempty
    IM_SB -> B.word8 b
    _ -> error ("Unhandled byte immediate encoding: " ++ show (ty, rex, oso))

encodeWordImmediate :: REX -> Bool -> Word16 -> OperandType -> B.Builder
encodeWordImmediate rex oso w ty =
  case ty of
    OpType ImmediateSource WSize -> B.word16LE w
    OpType ImmediateSource ZSize -> B.word16LE w
    OpType ImmediateSource VSize -> B.word16LE w
    IM_SB | L.view rexW rex -> B.word32LE (fromIntegral w)
          | otherwise -> B.word16LE w
    IM_SZ | L.view rexW rex -> B.word32LE (fromIntegral w)
          | otherwise -> B.word16LE w
    IM_1 -> mempty
    _ -> error ("Unhandled word immediate encoding: " ++ show (ty, rex, oso))

encodeDWordImmediate :: REX -> Bool -> Word32 -> OperandType -> B.Builder
encodeDWordImmediate rex oso dw ty =
  case ty of
    OpType ImmediateSource DSize -> B.word32LE dw
    OpType ImmediateSource VSize -> B.word32LE dw
    OpType ImmediateSource ZSize -> B.word32LE dw
    OpType OffsetSource DSize -> B.word32LE dw
    OpType OffsetSource VSize -> B.word32LE dw
    OpType OffsetSource ZSize -> B.word32LE dw
    IM_SZ -> B.word32LE dw
    IM_SB -> B.word8 (fromIntegral dw)
    IM_1 -> mempty
    _ -> error ("Unhandled dword immediate encoding: " ++ show (ty, rex, oso))

encodeQWordImmediate :: REX -> Bool -> Word64 -> OperandType -> B.Builder
encodeQWordImmediate rex oso qw ty =
  case ty of
    OpType ImmediateSource QSize -> B.word64LE qw
    OpType ImmediateSource VSize -> B.word64LE qw
    OpType OffsetSource VSize -> B.word64LE qw
    IM_SB -> B.word8 (fromIntegral qw)
    IM_1 -> mempty
    _ -> error ("Unhandled qword immediate encoding: " ++ show (ty, rex, oso))

{- Note [IM_1]

The IM_1 operand type denotes a constant 1.  This seems to be always
encoded as part of the opcode, and does not need to be rendered as an
argument.

-}

{- Note [x86 Instruction Format]

- Prefixes (0-4 bytes)
- Opcode (0-3 bytes)
- ModR/M (1 byte)
- SIB (1 byte)
- Displacement (0, 1, 2, or 4 bytes)
- Immediate (0, 1, 2, or 4 bytes)

Prefixes can occur in any order; preserving the order is not a huge
priority, though should not be difficult.

ModR/M refers to Mode-Register-Memory and describes the operation and
operands.  If it is missing, the REG field is encoded in the opcode.
Bits 7-6 (2 bits in the byte) represent 4 addressing modes:
* 11b = register-direct
* !11b = register indirect, displacement follows

Bits 5-3 indicate a register based operand or extended operation
encoding

Bits 2-0 are a register or memory operand when combined with mod


I think the general convention is: if there is a ModR/M, it almost
always encodes the first argument.  Sometimes, it acts as an extension
to the opcode (e.g., in mfence).

The immediate operand comes last (if there is one)



The ModR/M byte is:

----------------------------------------
|MOD (2 bits)|REG (3 bits)|R/M (2 bits)|
----------------------------------------

* MOD and R/M combine to specify a mem/reg operand and addressing mode

* REG is another register operand OR an opcode extension

That means that, at most, an instruction can reference two registers
and an immediate


Rules?

* If there are expected REG, MOD, or R/M values, fill them in verbatim
* Compute the rest of the byte based on operands.

-}
