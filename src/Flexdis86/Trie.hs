{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Flexdis86.Trie
  ( Vec8
  , generateM
  , index
  , indexM
  , Trie8(..)
  , mkTrie
  , mkTrieSorted
  , mkTrieFromBlob
  ) where

import qualified Control.DeepSeq as DS
import qualified Data.ByteString as BS
import           Data.Word (Word8)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import           GHC.Generics (Generic)

-- | A 'V.Vector' of exactly 256 elements.
--
-- Named analogously to 'Word8': 8 bits, 256 elements.
newtype Vec8 a = Vec8 (V.Vector a)
  deriving (Foldable, Functor, Generic, DS.NFData, Show)

-- Can truncate in general, but all uses in this module are safe
unsafeIntToWord8 :: Int -> Word8
unsafeIntToWord8 = fromIntegral
{-# INLINE unsafeIntToWord8 #-}

-- Always safe, in contrast to the above
word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral
{-# INLINE word8ToInt #-}

generate :: (Word8 -> a) -> Vec8 a
generate f = Vec8 (V.generate 256 (f . unsafeIntToWord8))
{-# INLINE generate #-}

generateM :: Monad m => (Word8 -> m a) -> m (Vec8 a)
generateM f = Vec8 <$> V.generateM 256 (f . unsafeIntToWord8)
{-# INLINE generateM #-}

-- | Index into the 'Vec8' to retrieve an element.
--
-- This uses unsafe indexing internally since we know:
--
-- 1. The underlying 'Vec.Vector' has exactly 256 elements (0x00 to 0xFF)
-- 2. 'Word8' values are always in the range @[0, 255]@
--
-- Therefore, the index is always in bounds and the bounds check is redundant.
index :: Vec8 a -> Word8 -> a
index (Vec8 vec) byte = V.unsafeIndex vec (word8ToInt byte)
{-# INLINE index #-}

-- | Monadic version of 'indexByteCache' that is strict in the vector.
indexM :: Monad m => Vec8 a -> Word8 -> m a
indexM (Vec8 vec) byte = V.unsafeIndexM vec (word8ToInt byte)
{-# INLINE indexM #-}

-- | Split entries based on first identifier in list of bytes.
partitionBy :: [([Word8], a)] -> Vec8 [([Word8], a)]
partitionBy l = Vec8 $ V.create $ do
  mv <- VM.replicate 256 []
  let  go ([], _d) = pure ()
       go (w:wl,d) = do
         el <- VM.read mv (word8ToInt w)
         VM.write mv (word8ToInt w) ((wl,d):el)
  mapM_ go l
  return mv

-- | A 256-way trie.
--
-- Named analogously to 'Word8': 8 bits, 256 elements.
data Trie8 a
   = Branch !(Vec8 (Trie8 a))
   | Leaf !a
  deriving (Generic, Show)

instance DS.NFData a => DS.NFData (Trie8 a)

-- | Construct a 'Trie8' from a list of byte-list prefixes and a leaf
-- constructor.
mkTrie :: ([a] -> b) -> [([Word8], a)] -> Trie8 b
mkTrie mkLeaf = go
  where
    emptyLeaf = Leaf (mkLeaf [])
    go l
      | all done l = Leaf (mkLeaf (snd <$> l))
      | otherwise  =
          let v = partitionBy l
              g i = case v `index` i of
                      [] -> emptyLeaf
                      bucket -> go bucket
          in Branch (generate g)
    done :: ([Word8], a) -> Bool
    done (remaining, _) = null remaining

-- | Like 'mkTrie' but assumes the input is already sorted lexicographically
-- by its 'BS.ByteString' keys.  Uses a linear group-by scan instead of
-- 'partitionBy'\'s mutable-vector, and shares a single @emptyLeaf@ value
-- across all vacant branches.
--
-- Keys are 'BS.ByteString' rather than @['Word8']@ so that callers can
-- pass zero-copy slices into an embedded @.rodata@ blob; each trie level
-- peels one byte with 'BS.uncons' (a pointer-bump on the slice header)
-- instead of following a cons-cell chain.
mkTrieSorted :: ([a] -> b) -> [(BS.ByteString, a)] -> Trie8 b
mkTrieSorted mkLeaf = go
  where
    emptyLeaf = Leaf (mkLeaf [])
    go l
      | all done l = Leaf (mkLeaf (map snd l))
      | otherwise  =
          Branch $ Vec8 $ V.create $ do
            mv <- VM.replicate 256 emptyLeaf
            mapM_ (\(b, bucket) ->
                    VM.write mv (word8ToInt b) (go bucket))
                  (groupByFirstByte l)
            return mv
    done :: (BS.ByteString, a) -> Bool
    done (remaining, _) = BS.null remaining

-- | Group a sorted @[('BS.ByteString', a)]@ list by the first byte of each
-- key, stripping that byte from every entry in the group.  Entries with an
-- empty key are skipped (they are already handled by the 'done' branch in
-- 'mkTrieSorted').
--
-- Uses an accumulator instead of 'span' to avoid materialising the same
-- entries twice.  The bucket is accumulated in reverse order, matching
-- 'partitionBy'.
groupByFirstByte :: [(BS.ByteString, a)] -> [(Word8, [(BS.ByteString, a)])]
groupByFirstByte = go
  where
    go []             = []
    go ((bs, d):rest) = case BS.uncons bs of
      Nothing      -> go rest
      Just (w, ws) -> collect w [(ws, d)] rest

    collect w acc []             = [(w, acc)]
    collect w acc ((bs, d):rest) = case BS.uncons bs of
      Nothing        -> collect w acc rest
      Just (w', ws')
        | w' == w    -> collect w ((ws', d):acc) rest
        | otherwise  -> (w, acc) : go ((bs, d):rest)

-- | Build a trie directly from a binary blob of sorted entries, without
-- materialising an intermediate list.
--
-- Each entry at offset @off@ in the blob is encoded as:
--
-- @
--   keyLen :: Word8          (1 byte)
--   key    :: [Word8]        (keyLen bytes)
--   ...payload...            (opaque to this function)
-- @
--
-- The caller provides a 'VU.Vector' of entry-start offsets (pointing at the
-- @keyLen@ byte) and a @mkVal@ callback that constructs a leaf value from an
-- offset.  The trie is built by grouping entries on their key byte at the
-- current depth — a contiguous-range scan over the offset vector — so the
-- only heap objects allocated during construction are the trie nodes
-- themselves and the leaf-value lists.
mkTrieFromBlob
  :: ([a] -> b)        -- ^ Construct a leaf from the values at a trie node
  -> BS.ByteString     -- ^ Binary blob
  -> (Int -> a)        -- ^ Construct a value from a blob offset
  -> VU.Vector Int     -- ^ Sorted entry-start offsets into the blob
  -> Trie8 b
mkTrieFromBlob mkLeaf blob mkVal offsets = go 0 0 (VU.length offsets)
  where
    emptyLeaf = Leaf (mkLeaf [])

    keyLen off = word8ToInt (BS.index blob off)
    {-# INLINE keyLen #-}

    keyByte off depth = BS.index blob (off + 1 + depth)
    {-# INLINE keyByte #-}

    go depth lo hi
      | lo >= hi
      = emptyLeaf
      | depth >= keyLen (offsets `VU.unsafeIndex` lo)
      = Leaf (mkLeaf [mkVal (offsets `VU.unsafeIndex` i) | i <- [lo .. hi - 1]])
      | otherwise
      = Branch $ Vec8 $ V.create $ do
          mv <- VM.replicate 256 emptyLeaf
          let scan i
                | i >= hi   = return ()
                | otherwise = do
                    let off = offsets `VU.unsafeIndex` i
                        b   = keyByte off depth
                        findEnd k
                          | k >= hi = k
                          | keyByte (offsets `VU.unsafeIndex` k) depth == b = findEnd (k + 1)
                          | otherwise = k
                        j = findEnd (i + 1)
                    VM.write mv (word8ToInt b) (go (depth + 1) i j)
                    scan j
          scan lo
          return mv

