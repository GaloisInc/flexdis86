{-# LANGUAGE ViewPatterns #-}
module Flexdis86.Assembler (
  assembleInstruction
  ) where

import Control.Applicative
import qualified Control.Lens as L
import Control.Monad ( MonadPlus(..) )
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import Data.Maybe ( catMaybes, fromMaybe, mapMaybe, maybeToList )

import Flexdis86.OpTable ( ModConstraint(..), unFin8 )
import Flexdis86.InstructionSet
import Flexdis86.Prefixes
import Flexdis86.Register
import Debug.Trace
debug = flip trace
assembleInstruction :: (MonadPlus m) => InstructionInstance -> m B.ByteString
assembleInstruction ii = do
  return $ B.concat $ concat [ prefixBytes
                             , [opcode]
                             , maybeToList (fmap B.singleton (encodeModRM ii))
                             , mapMaybe encodeImmediate (iiArgs ii)
                             ]
  where
    prefixBytes = catMaybes [ encodeLockPrefix (L.view prLockPrefix pfxs)
                            , if L.view prOSO pfxs then Just (B.singleton 0x66) else Nothing
                            , if L.view prASO pfxs then Just (B.singleton 0x67) else Nothing
                            , encodeREXPrefix (L.view prREX pfxs)
                            , encodeRequiredPrefix (iiRequiredPrefix ii)
                            ]
    opcode = B.pack (iiOpcode ii)
    pfxs = iiPrefixes ii

encodeModRM :: (Alternative m) => InstructionInstance -> m Word8
encodeModRM ii
  | not (iiHasModRM ii) = empty
  | otherwise = encodeRequiredModRM ii <|> encodeOperandModRM ii <|> empty

-- | Build a ModRM byte based on the operands of the instruction.
encodeOperandModRM :: (Alternative m) => InstructionInstance -> m Word8
encodeOperandModRM ii =
  case iiArgs ii of
    [ByteReg r8] -> pure $ mkModRM directRegister 0 (reg8ToRM r8) `debug` show ("byte", r8, reg8ToRM r8)
    [WordReg (Reg16 rno)] -> pure $ mkModRM directRegister 0 rno
    [DWordReg (Reg32 rno)] -> pure $ mkModRM directRegister 0 rno `debug` show ("dword", rno, mkModRM directRegister 0 rno)
    [QWordReg (Reg64 rno)] -> pure $ mkModRM directRegister 0 rno

-- | We represent the high registers (e.g., ah) as 16+raxno.
--
-- We need to remove the 16 we added (hence the 0xf mask).  Further,
-- x86 represents the high registers starting ah at 4.
reg8ToRM :: Reg8 -> Word8
reg8ToRM (Reg8 rno)
  | rno >= 16 = rno .&. 0xf + 4
  | otherwise = rno

-- | This is the "direct register" addressing method with the value
-- already shifted appropriately.
directRegister :: Word8
directRegister = 3 `shiftL` 6

-- | From constituent components, construct the ModRM byte with
-- appropriate shifting.
mkModRM :: Word8 -> Word8 -> Word8 -> Word8
mkModRM modb regb rmb = modb .|. regb .|. rmb

-- | Build a ModRM byte from a full set of entirely specified mod/rm
-- bits.
--
-- If there are no arguments to the instruction, we can take a partial
-- set of mod/reg/rm values and compute the byte with zeros for the
-- missing values.
encodeRequiredModRM :: (Alternative m) => InstructionInstance -> m Word8
encodeRequiredModRM ii =
  case (fmap ((`shiftL` 6) . modConstraintVal) (iiRequiredMod ii),
        fmap ((`shiftL` 3) . unFin8) (iiRequiredReg ii),
        fmap unFin8 (iiRequiredRM ii)) of
    (Nothing, Nothing, Nothing) -> empty
    (Just rmod, Just rreg, Just rrm) ->
      pure $ rmod .|. rreg .|. rrm
    (mmod, mreg, mrm)
      | null (iiArgs ii) ->
          pure $ fromMaybe 0 mmod .|. fromMaybe 0 mreg .|. fromMaybe 0 mrm
      | otherwise -> empty



modConstraintVal :: ModConstraint -> Word8
modConstraintVal mc =
  case mc of
    OnlyReg -> 3
    OnlyMem -> 0

encodeRequiredPrefix :: Maybe Word8 -> Maybe B.ByteString
encodeRequiredPrefix = fmap B.singleton

encodeLockPrefix :: LockPrefix -> Maybe B.ByteString
encodeLockPrefix pfx =
  case pfx of
    NoLockPrefix -> Nothing
    LockPrefix -> Just (B.singleton 0xF0)
    RepNZPrefix -> Just (B.singleton 0xF2)
    RepPrefix -> Just (B.singleton 0xF3)
    RepZPrefix -> Just (B.singleton 0xF3)

-- | Right now, a zero REX prefix is ignored and any other value is
-- returned directly.  Not entirely sure if that is correct, but it
-- seems to work okay.
encodeREXPrefix :: REX -> Maybe B.ByteString
encodeREXPrefix (REX rex) | rex == 0 = Nothing
                          | otherwise = Just (B.singleton rex)

encodeImmediate :: Value -> Maybe B.ByteString
encodeImmediate v =
  case v of
    ByteImm imm -> Just $ B.singleton imm
    WordImm imm -> Just $ LB.toStrict $ B.toLazyByteString $ B.word16LE imm
    DWordImm imm -> Just $ LB.toStrict $ B.toLazyByteString $ B.word32LE imm
    QWordImm imm -> Just $ LB.toStrict $ B.toLazyByteString $ B.word64LE imm
    _ -> Nothing

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
