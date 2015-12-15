module Flexdis86.Assembler (
  assembleInstruction
  ) where

import qualified Control.Lens as L
import Control.Monad ( MonadPlus(..) )
import qualified Data.ByteString as B
import Data.Maybe ( mapMaybe, maybeToList )

import Flexdis86.InstructionSet
import Flexdis86.OpTable

assembleInstruction :: (MonadPlus m) => InstructionInstance -> m B.ByteString
assembleInstruction ii = do
  return $ B.concat $ concat [ lockPrefixes
                             , reqPrefix
                             , [opcode]
                             ]
  where
    lockPrefixes = maybeToList (encodeLockPrefix (iiLockPrefix ii))
    reqPrefix = maybeToList (encodeRequiredPrefix (L.view requiredPrefix enc))
    opcode = B.pack (L.view defOpcodes enc)
--    modrm = maybeToList (encodeModRM (L.view requiredMod enc) (L.view requiredReg enc) (L.view requiredRM enc))
    enc = iiEncoding ii

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


-}
