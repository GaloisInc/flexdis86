{- |
Module      : Flexdis86.OpcodeList
Copyright   : (c) Galois, Inc, 2026
Maintainer  : langston@galois.com

A compact Word32 representation of an x86 opcode byte sequence
(1–3 bytes), replacing @[Word8]@ in 'Flexdis86.OpTable.Def'.

Encoding (all fields packed into 32 bits):

  * bits  0– 7: byte 0 (always present)
  * bits  8–15: byte 1 (present when length ≥ 2)
  * bits 16–23: byte 2 (present when length ≥ 3)
  * bits 24–25: length (1, 2, or 3; 0 means empty, used only during parsing)
  * bits 26–31: unused
-}
{-# LANGUAGE DeriveGeneric #-}
module Flexdis86.OpcodeList
  ( OpcodeList
  , emptyOpcodeList
  , snocOpcodeList
  , consOpcodeList
  , singletonOpcodeList
  , opcodeListToList
  , opcodeListLength
  ) where

import           Data.Bits
import           Data.Binary (Binary)
import qualified Control.DeepSeq as DS
import           Data.Word (Word8, Word32)
import           GHC.Generics (Generic)

-- | A packed sequence of 0–3 opcode bytes.
newtype OpcodeList = OpcodeList Word32
  deriving (Eq, Generic, Ord, Show)

instance DS.NFData OpcodeList
-- | For "Flexdis86.OpTable.Parse".
instance Binary OpcodeList

-- | Empty sequence (used only during incremental parsing).
emptyOpcodeList :: OpcodeList
emptyOpcodeList = OpcodeList 0

-- | Number of bytes stored (0–3).
opcodeListLength :: OpcodeList -> Int
opcodeListLength (OpcodeList w) = fromIntegral ((w `shiftR` 24) .&. 3)
{-# INLINE opcodeListLength #-}

-- | Append a byte at the high end.
snocOpcodeList :: OpcodeList -> Word8 -> OpcodeList
snocOpcodeList ol@(OpcodeList w) b =
  let len = opcodeListLength ol
      newByte = fromIntegral b `shiftL` (8 * len)
      newLen  = fromIntegral (len + 1) `shiftL` 24
  in OpcodeList ((w .&. 0x00FFFFFF) .|. newByte .|. newLen)
{-# INLINE snocOpcodeList #-}

-- | Prepend a byte at the low end (shifts existing bytes up).
consOpcodeList :: Word8 -> OpcodeList -> OpcodeList
consOpcodeList b ol@(OpcodeList w) =
  let len    = opcodeListLength ol
      newLen = fromIntegral (len + 1) `shiftL` 24
      shifted = (w .&. 0x00FFFFFF) `shiftL` 8
  in OpcodeList (newLen .|. shifted .|. fromIntegral b)
{-# INLINE consOpcodeList #-}

-- | A one-element 'OpcodeList'.
singletonOpcodeList :: Word8 -> OpcodeList
singletonOpcodeList b = OpcodeList ((1 `shiftL` 24) .|. fromIntegral b)
{-# INLINE singletonOpcodeList #-}

-- | Convert to a list of bytes (length 0–3).
opcodeListToList :: OpcodeList -> [Word8]
opcodeListToList ol@(OpcodeList w) =
  [ fromIntegral (w `shiftR` (8 * i)) | i <- [0 .. opcodeListLength ol - 1] ]
{-# INLINE opcodeListToList #-}
