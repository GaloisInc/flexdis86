{-# LANGUAGE ViewPatterns #-}
module Flexdis86.Assembler (
  assembleInstruction
  ) where

import Control.Applicative
import qualified Control.Lens as L
import Control.Monad ( MonadPlus(..) )
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as B
import Data.Maybe ( catMaybes, fromMaybe, mapMaybe )
import Data.Monoid

import Prelude

import Flexdis86.OpTable ( ModConstraint(..), unFin8 )
import Flexdis86.InstructionSet
import Flexdis86.Prefixes
import Flexdis86.Register
import Debug.Trace
debug = flip trace
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
  | not (iiHasModRM ii) = empty
  | otherwise = encodeRequiredModRM ii <|> encodeOperandModRM ii <|> empty

-- | Build a ModRM byte based on the operands of the instruction.
encodeOperandModRM :: (Alternative m) => InstructionInstance -> m B.Builder
encodeOperandModRM ii =
  case filter isNotImmediate (iiArgs ii) of
    [] -> empty
    [op1] ->
      pure $ withMode op1 $ \mode disp ->
        mkModRM mode 0 (encodeValue op1) <> disp
      -- pure $ B.singleton $ mkModRM (mkMode op1) 0 (encodeValue op1)
    [op1, op2] ->
      pure $ withMode op1 $ \mode disp ->
        mkModRM mode (encodeValue op2) (encodeValue op1) <> disp
      -- pure $ B.singleton $ mkModRM (mkMode op1) (encodeValue op2) (encodeValue op1)
    _ -> empty

-- | Compute the mode bits and displacement for a 'Value'.
-- Eventually, this will also compute the SIB.
--
-- The three are all very closely related, so computing them all at
-- once makes the most sense.
withMode :: Value -> (Word8 -> B.Builder -> a) -> a
withMode v k =
  case v `debug` show ("mode", v) of
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

    -- Mem64 (Addr_64 _ (Just _) _ NoDisplacement) -> k noDisplacement Nothing
    -- Mem32 (Addr_64 _ (Just _) _ NoDisplacement) -> k noDisplacement Nothing
    -- Mem16 (Addr_64 _ (Just _) _ NoDisplacement) -> k noDisplacement Nothing
    -- Mem8 (Addr_64 _ (Just _) _ NoDisplacement) -> k noDisplacement Nothing
    -- Mem64 (Addr_32 _ (Just _) _ NoDisplacement) -> k noDisplacement Nothing
    -- Mem32 (Addr_32 _ (Just _) _ NoDisplacement) -> k noDisplacement Nothing
    -- Mem16 (Addr_32 _ (Just _) _ NoDisplacement) -> k noDisplacement Nothing
    -- Mem8 (Addr_32 _ (Just _) _ NoDisplacement) -> k noDisplacement Nothing
    _ -> error "mkMode: Unsupported mode"

-- | This is the "direct register" addressing method with the value
-- already shifted appropriately.
directRegister :: Word8
directRegister = 3

noDisplacement :: Word8
noDisplacement = 0

encodeValue :: Value -> Word8
encodeValue v =
  case v of
    ByteReg r8 -> reg8ToRM r8
    WordReg (Reg16 rno) -> rno
    DWordReg (Reg32 rno) -> rno
    QWordReg (Reg64 rno) -> rno
    Mem64 (Addr_64 _ (Just (Reg64 rno)) _ _) -> rno
    Mem32 (Addr_64 _ (Just (Reg64 rno)) _ _) -> rno
    Mem16 (Addr_64 _ (Just (Reg64 rno)) _ _) -> rno
    Mem8 (Addr_64 _ (Just (Reg64 rno)) _ _) -> rno
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
