{- |
Module      :  $Header$
Copyright   :  (c) Galois, Inc 2013-2016
Maintainer  :  jhendrix@galois.com

This declares a typeclass and utility functions for reading binary data
with least-significant byte first.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Flexdis86.ByteReader
  ( ByteReader(..)
  ) where

import Control.Applicative
import Data.Binary.Get (Get, getWord8)
import Data.Bits
import Data.Int
import Data.Word

import Flexdis86.Relocation

import Prelude

-- | @readAndShift reader i@ invokes reader twice, the first one is stored
-- in the low-order bits and the second is stored in the high order bits.
readLSB' :: (Monad m, Bits b, Num b)
         => b -- ^ Accumulator
         -> Int -- ^ Number of bytes read so far
         -> m Word8 -- ^ Reader
         -> Int  -- ^ Number of bits to read
         -> m b
readLSB' cur cnt reader n
  | cnt < n = do
      w <- fromIntegral <$> reader
      let cur' = cur .|. w `shiftL` cnt
      let cnt' = cnt + 8
      seq cur' $ seq cnt' $ readLSB' cur' cnt' reader n
  | otherwise = do
      pure $! cur


-- | @readLSB w n@ reads a @n@-byte value stored with least-significant
-- byte first using the reader @w@ for reading bytes.
readLSB :: (Monad m, Bits b, Num b)
         => m Word8 -- ^ Reader
         -> Int  -- ^ Number of bits to read
         -> m b
readLSB = readLSB' 0 0

-- | A reader monad for reading values from a stream.
class (Applicative m, Monad m) => ByteReader m where
  -- | Read a byte.
  readByte :: m Word8

  -- | Read a 16-bit value with the least-significant byte first.
  readWord :: m Word16
  readWord = readLSB readByte 2

  -- | Read a 32-bit value with the least-significant byte first.
  readDImm :: m Imm32
  readDImm = Imm32Concrete <$> readLSB readByte 4

  -- | Read a 64-bit value with the least-significant byte first.
  readQWord :: m Word64
  readQWord = readLSB readByte 8

  -- | Invalid instruction when parsing
  invalidInstruction :: m a
  invalidInstruction = fail "Invalid instruction"

  readSByte :: m Int8
  readSByte  = fromIntegral <$> readByte

  readSWord :: m Int16
  readSWord  = fromIntegral <$> readWord

  readSDWord :: m Int32
  readSDWord = readLSB readByte 4

  -- | This reads a jump offset with the given number of bytes
  readJump :: JumpSize -> m JumpOffset
  readJump sz =
    case sz of
      JSize8  -> FixedOffset . fromIntegral <$> readSByte
      JSize16 -> FixedOffset . fromIntegral <$> readSWord
      JSize32 -> FixedOffset . fromIntegral <$> readSDWord

instance ByteReader Get where
  readByte = getWord8
