{- |
Module      :  $Header$
Copyright   :  (c) Galois, Inc 2013-2016
Maintainer  :  jhendrix@galois.com

This declares a typeclass and utility functions for reading binary data
with least-significant byte first.
-}
module Flexdis86.ByteReader
  ( ByteReader(..)
  , readSByte
  , readSWord
  , readSDWord
  , readSQWord
  ) where

import Control.Applicative
import Data.Binary.Get (Get, getWord8)
import Data.Bits
import Data.Int
import Data.Word

import Prelude

-- | A reader monad for reading values from a stream.
class (Applicative m, Monad m) => ByteReader m where
  -- | Read a byte.
  readByte :: m Word8

  -- | Read a 16-bit value with the least-significant byte first.
  readWord :: m Word16
  readWord = readAndShift readByte 8

  -- | Read a 32-bit value with the least-significant byte first.
  readDWord :: m Word32
  readDWord = readAndShift readWord 16

  -- | Read a 64-bit value with the least-significant byte first.
  readQWord :: m Word64
  readQWord = readAndShift readDWord 32

  -- | Invalid instruction when parsing
  invalidInstruction :: m a
  invalidInstruction = fail "Invalid instruction"

-- | @readAndShift reader i@ invokes reader twice, the first one is stored
-- in the low-order bits and the second is stored in the high order bits.
readAndShift :: (Applicative m, Integral a, Bits b, Num b)
             => m a
             -> Int
             -> m b
readAndShift reader i = (.|.) <$> reader' <*> ((`shiftL` i) <$> reader')
  where reader' = fromIntegral <$> reader

readSByte :: ByteReader m => m Int8
readSByte  = fromIntegral <$> readByte

readSWord :: ByteReader m => m Int16
readSWord  = fromIntegral <$> readWord

readSDWord :: ByteReader m => m Int32
readSDWord = fromIntegral <$> readDWord

readSQWord :: ByteReader m => m Int64
readSQWord = fromIntegral <$> readQWord

instance ByteReader Get where
  readByte = getWord8
