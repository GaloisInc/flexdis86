{-# LANGUAGE ViewPatterns #-}
module Flexdis86.Assembler (
  AssemblerContext,
  assemblerContext,
  mkInstruction,
  assembleInstruction
  ) where

import Control.Applicative
import Control.Arrow ( second )
import qualified Control.Lens as L
import Control.Monad ( MonadPlus(..) )
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.Map.Strict as M
import Data.Maybe ( fromMaybe, isJust )
import Data.Monoid

import Prelude

import Flexdis86.OpTable
import Flexdis86.InstructionSet
import Flexdis86.Prefixes
import Flexdis86.Register

data AssemblerContext =
  AssemblerContext { acDefs :: M.Map String [Def]
                   }
  deriving (Show)

assemblerContext :: [Def] -> AssemblerContext
assemblerContext = AssemblerContext . foldr addDef M.empty
  where
    addDef d = M.alter (Just . (maybe [d] (d:))) (L.view defMnemonic d)

mkInstruction :: (Alternative f)
              => AssemblerContext
              -> String
              -- ^ Mnemonic
              -> [Value]
              -- ^ Arguments
              -> f InstructionInstance
mkInstruction ctx mnemonic args =
  foldr (<|>) empty (map (findEncoding args) defs)
  where
    defs = fromMaybe [] $ M.lookup mnemonic (acDefs ctx)

findEncoding :: (Alternative f) => [Value] -> Def -> f InstructionInstance
findEncoding args def
  | not (argumentsCongruent args def) = empty
  | otherwise = pure $ II { iiLockPrefix = NoLockPrefix
                          , iiAddrSize = Size16
                          , iiOp = L.view defMnemonic def
                          , iiArgs = args
                          , iiPrefixes = Prefixes { _prLockPrefix = NoLockPrefix
                                                  , _prSP = no_seg_prefix
                                                  , _prREX = REX 0
                                                  , _prASO = False
                                                  , _prOSO = False
                                                  }
                          , iiRequiredPrefix = L.view requiredPrefix def
                          , iiOpcode = L.view defOpcodes def
                          , iiRequiredMod = L.view requiredMod def
                          , iiRequiredReg = L.view requiredReg def
                          , iiRequiredRM = L.view requiredRM def
                          }

-- Need to build prefixes based on arg sizes...

-- | Return True if the value list matches the operand types supported
-- by the given 'Def'.
argumentsCongruent :: [Value] -> Def -> Bool
argumentsCongruent args def =
  length args == length opTypes && all matchOperandType (zip opTypes args)
  where
    opTypes = L.view defOperands def

-- | Return True if the given 'Value' can be encoded with the operand
-- type (specified as a String).
matchOperandType :: (String, Value) -> Bool
matchOperandType ops =
  case ops of
    ("Ib", ByteImm _) -> True
    ("Iw", WordImm _) -> True
    _ -> False

assembleInstruction :: (MonadPlus m) => InstructionInstance -> m B.Builder
assembleInstruction ii = do
  return $ mconcat [ prefixBytes
                   , opcode
                   , fromMaybe mempty (encodeModRMDisp ii)
                   , mconcat (map encodeImmediate (iiArgs ii))
                   ]
  where
    prefixBytes = mconcat [ encodeLockPrefix (L.view prLockPrefix pfxs)
                          , if L.view prASO pfxs then B.word8 0x67 else mempty
                          , if L.view prOSO pfxs then B.word8 0x66 else mempty
                          , encodeREXPrefix (L.view prREX pfxs)
                          , encodeRequiredPrefix (iiRequiredPrefix ii)
                          ]
    opcode = B.byteString (B.pack (iiOpcode ii))
    pfxs = iiPrefixes ii

-- | Construct the ModR/M, SIB, and Displacement bytes
--
-- The arguments all determine these together, so we really need to
-- build a combined byte string.
encodeModRMDisp :: (Alternative m) => InstructionInstance -> m B.Builder
encodeModRMDisp ii
  | not (hasModRM ii) = empty
  | otherwise = encodeRequiredModRM ii <|> encodeOperandModRM ii <|> empty

hasModRM :: InstructionInstance -> Bool
hasModRM ii = or [ isJust (iiRequiredMod ii)
                 , isJust (iiRequiredReg ii)
                 , isJust (iiRequiredRM ii)
                 , any isNotImmediate (iiArgs ii)
                 ]

-- | Build a ModRM byte based on the operands of the instruction.
encodeOperandModRM :: (Alternative m) => InstructionInstance -> m B.Builder
encodeOperandModRM ii =
  case filter isNotImmediate (iiArgs ii) of
    [] -> empty
    [op1] ->
      pure $ withMode op1 $ \mode disp ->
        let rm = encodeValue op1
        in withSIB mode rm op1 $ \sib ->
             mkModRM mode 0 rm <> sib <> disp
    [op1, op2] ->
      pure $ withMode op1 $ \mode disp ->
        let rm = encodeValue op1
        in withSIB mode rm op1 $ \sib ->
             mkModRM mode (encodeValue op2) rm <> sib <> disp
    _ -> empty

-- | Provide the SIB to the callback, if it is required
withSIB :: Word8 -> Word8 -> Value -> (B.Builder -> a) -> a
withSIB mode rm val k
  | not (requiresSIB rm) || mode == directRegister = k mempty
  | otherwise =
    case val of
      Mem8 (Addr_32 _seg mbase midx _) ->
        let midx' = fmap (second (unReg64 . reg32_reg)) midx
            mbase' = fmap (unReg64 . reg32_reg) mbase
        in k (mkSIB midx' mbase')
      Mem8 (Addr_64 _seg mbase midx _) ->
        let midx' = fmap (second unReg64) midx
            mbase' = fmap unReg64 mbase
        in k (mkSIB midx' mbase')
      _ -> k mempty

mkSIB :: Maybe (Int, Word8)
         -- ^ (optional) (2^Scale, Index)
      -> Maybe Word8
         -- ^ Register base
      -> B.Builder
mkSIB mScaleIdx mBase =
  case (mScaleIdx, mBase) of
    (Nothing, Just rno) -> B.word8 ((4 `shiftL` 3) .|. (rno .&. 0x7))
    (Just (scale, ix), Just rno) ->
      B.word8 ((round (logBase 2 (fromIntegral scale) :: Double) `shiftL` 6) .|. (ix `shiftL` 3) .|. (rno .&. 0x7))
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

    Mem64 (Addr_64 _ (Just _) _ NoDisplacement) -> k noDisplacement mempty
    Mem32 (Addr_64 _ (Just _) _ NoDisplacement) -> k noDisplacement mempty
    Mem16 (Addr_64 _ (Just _) _ NoDisplacement) -> k noDisplacement mempty
    Mem8 (Addr_64 _ (Just _) _ NoDisplacement) -> k noDisplacement mempty
    Mem64 (Addr_32 _ (Just _) _ NoDisplacement) -> k noDisplacement mempty
    Mem32 (Addr_32 _ (Just _) _ NoDisplacement) -> k noDisplacement mempty
    Mem16 (Addr_32 _ (Just _) _ NoDisplacement) -> k noDisplacement mempty
    Mem8 (Addr_32 _ (Just _) _ NoDisplacement) -> k noDisplacement mempty

    Mem64 (Addr_64 _ (Just _) _ (Disp32 d)) -> k disp32 (B.int32LE d)
    Mem32 (Addr_64 _ (Just _) _ (Disp32 d)) -> k disp32 (B.int32LE d)
    Mem16 (Addr_64 _ (Just _) _ (Disp32 d)) -> k disp32 (B.int32LE d)
    Mem8 (Addr_64 _ (Just _) _ (Disp32 d)) -> k disp32 (B.int32LE d)
    Mem64 (Addr_32 _ (Just _) _ (Disp32 d)) -> k disp32 (B.int32LE d)
    Mem32 (Addr_32 _ (Just _) _ (Disp32 d)) -> k disp32 (B.int32LE d)
    Mem16 (Addr_32 _ (Just _) _ (Disp32 d)) -> k disp32 (B.int32LE d)
    Mem8 (Addr_32 _ (Just _) _ (Disp32 d)) -> k disp32 (B.int32LE d)

    Mem64 (Addr_64 _ (Just _) _ (Disp8 d)) -> k disp8 (B.int8 d)
    Mem32 (Addr_64 _ (Just _) _ (Disp8 d)) -> k disp8 (B.int8 d)
    Mem16 (Addr_64 _ (Just _) _ (Disp8 d)) -> k disp8 (B.int8 d)
    Mem8 (Addr_64 _ (Just _) _ (Disp8 d)) -> k disp8 (B.int8 d)
    Mem64 (Addr_32 _ (Just _) _ (Disp8 d)) -> k disp8 (B.int8 d)
    Mem32 (Addr_32 _ (Just _) _ (Disp8 d)) -> k disp8 (B.int8 d)
    Mem16 (Addr_32 _ (Just _) _ (Disp8 d)) -> k disp8 (B.int8 d)
    Mem8 (Addr_32 _ (Just _) _ (Disp8 d)) -> k disp8 (B.int8 d)
    _ -> error "mkMode: Unsupported mode"

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

encodeValue :: Value -> Word8
encodeValue v =
  case v of
    ByteReg r8 -> reg8ToRM r8
    WordReg (Reg16 rno) -> rno
    DWordReg (Reg32 rno) -> rno
    QWordReg (Reg64 rno) -> rno
    Mem64 (Addr_64 _ (Just (Reg64 rno)) _ _) -> 0x7 .&. rno
    Mem32 (Addr_64 _ (Just (Reg64 rno)) _ _) -> 0x7 .&. rno
    Mem16 (Addr_64 _ (Just (Reg64 rno)) _ _) -> 0x7 .&. rno
    Mem8 (Addr_64 _ (Just (Reg64 rno)) _ _) -> 0x7 .&. rno
    Mem64 (Addr_32 _ (Just (Reg32 rno)) _ _) -> rno
    Mem32 (Addr_32 _ (Just (Reg32 rno)) _ _) -> rno
    Mem16 (Addr_32 _ (Just (Reg32 rno)) _ _) -> rno
    Mem8 (Addr_32 _ (Just (Reg32 rno)) _ _) -> rno

isNotImmediate :: Value -> Bool
isNotImmediate val =
  case val of
    ByteImm {} -> False
    WordImm {} -> False
    DWordImm {} -> False
    QWordImm {} -> False
    _ -> True

-- | We represent the high registers (e.g., ah) as 16+regnum.
--
-- We need to remove the 16 we added (hence the 0xf mask).  Further,
-- x86 represents the high registers starting ah at 4.
reg8ToRM :: Reg8 -> Word8
reg8ToRM (Reg8 rno)
  | rno >= 16 = rno .&. 0xf + 4
  | otherwise = rno

-- | From constituent components, construct the ModRM byte with
-- appropriate shifting.
mkModRM :: Word8 -> Word8 -> Word8 -> B.Builder
mkModRM modb regb rmb = B.word8 ((modb `shiftL` 6) .|. (regb `shiftL` 3) .|. rmb)

-- | Build a ModRM byte from a full set of entirely specified mod/rm
-- bits.
--
-- If there are no arguments to the instruction, we can take a partial
-- set of mod/reg/rm values and compute the byte with zeros for the
-- missing values.
encodeRequiredModRM :: (Alternative m) => InstructionInstance -> m B.Builder
encodeRequiredModRM ii =
  case (fmap ((`shiftL` 6) . modConstraintVal) (iiRequiredMod ii),
        fmap ((`shiftL` 3) . unFin8) (iiRequiredReg ii),
        fmap unFin8 (iiRequiredRM ii)) of
    (Nothing, Nothing, Nothing) -> empty
    (Just rmod, Just rreg, Just rrm) ->
      -- FIXME: Does this case require shifting?  I think it might, if
      -- the values are pulled right from the XML file.  On the other
      -- hand, the tests with required bytes seem to pass (see the
      -- mfence tests)
      pure $ B.word8 (rmod .|. rreg .|. rrm)
    (mmod, mreg, mrm)
      | null (iiArgs ii) ->
          pure $ B.word8 (fromMaybe 0 mmod .|. fromMaybe 0 mreg .|. fromMaybe 0 mrm)
      | otherwise -> empty

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
                          | otherwise = B.word8 rex

encodeImmediate :: Value -> B.Builder
encodeImmediate v =
  case v of
    ByteImm imm -> B.word8 imm
    WordImm imm -> B.word16LE imm
    DWordImm imm -> B.word32LE imm
    QWordImm imm -> B.word64LE imm
    _ -> mempty



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
