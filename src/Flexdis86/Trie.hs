{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Flexdis86.Trie
  ( Vec8
  , generateM
  , index
  , Trie8(..)
  , mkTrie
  ) where

import qualified Control.DeepSeq as DS
import Data.Bits (popCount, testBit, shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8, Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.Generics (Generic)

------------------------------------------------------------------------
-- Bitmap256

-- | A 256-bit bitmap stored as four 'Word64's.
data Bitmap256 = Bitmap256 !Word64 !Word64 !Word64 !Word64
  deriving (Generic, Show)

instance DS.NFData Bitmap256

-- | A 'Bitmap256' with all 256 bits set.
fullBitmap256 :: Bitmap256
fullBitmap256 = Bitmap256 maxBound maxBound maxBound maxBound
{-# INLINE fullBitmap256 #-}

-- | Test whether bit @b@ is set.
testBit256 :: Bitmap256 -> Word8 -> Bool
testBit256 (Bitmap256 w0 w1 w2 w3) b =
  let i = word8ToInt b
      q = i `shiftR` 6
      r = i .&. 63
  in testBit (case q of { 0 -> w0; 1 -> w1; 2 -> w2; _ -> w3 }) r
{-# INLINE testBit256 #-}

-- | Count the number of set bits strictly below position @b@.
-- This gives the index into the compact array for a present entry.
sparseIndex :: Bitmap256 -> Word8 -> Int
sparseIndex (Bitmap256 w0 w1 w2 w3) b =
  let i = word8ToInt b
      q = i `shiftR` 6
      r = i .&. 63
      mask = (1 `shiftL` r) - 1
  in case q of
       0 -> popCount (w0 .&. mask)
       1 -> popCount w0 + popCount (w1 .&. mask)
       2 -> popCount w0 + popCount w1 + popCount (w2 .&. mask)
       _ -> popCount w0 + popCount w1 + popCount w2 + popCount (w3 .&. mask)
{-# INLINE sparseIndex #-}

-- | Build a bitmap from a predicate over byte values.
buildBitmap :: (Word8 -> Bool) -> Bitmap256
buildBitmap p = Bitmap256 (mkWord 0) (mkWord 64) (mkWord 128) (mkWord 192)
  where
    mkWord :: Int -> Word64
    mkWord base = go 0 0
      where
        go :: Word64 -> Int -> Word64
        go !acc 64 = acc
        go !acc !bit
          | p (unsafeIntToWord8 (base + bit)) = go (acc .|. (1 `shiftL` bit)) (bit + 1)
          | otherwise = go acc (bit + 1)

------------------------------------------------------------------------
-- Vec8

-- | A sparse vector indexed by 'Word8', backed by a bitmap and a
-- compact array of only the present entries.
--
-- Named analogously to 'Word8': 8 bits, up to 256 elements.
data Vec8 a = Vec8 !Bitmap256 !(V.Vector a)
  deriving (Generic, Show)

instance Functor Vec8 where
  fmap f (Vec8 bm arr) = Vec8 bm (V.map f arr)

instance Foldable Vec8 where
  foldr f z (Vec8 _ arr) = V.foldr f z arr
  foldl' f z (Vec8 _ arr) = V.foldl' f z arr
  length (Vec8 _ arr) = V.length arr
  null (Vec8 _ arr) = V.null arr

instance DS.NFData a => DS.NFData (Vec8 a)

-- Can truncate in general, but all uses in this module are safe
unsafeIntToWord8 :: Int -> Word8
unsafeIntToWord8 = fromIntegral
{-# INLINE unsafeIntToWord8 #-}

-- Always safe, in contrast to the above
word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral
{-# INLINE word8ToInt #-}

generateM :: Monad m => (Word8 -> m a) -> m (Vec8 a)
generateM f = Vec8 fullBitmap256 <$> V.generateM 256 (f . unsafeIntToWord8)
{-# INLINE generateM #-}

-- | Look up a byte index.  Returns 'Nothing' for absent entries.
index :: Vec8 a -> Word8 -> Maybe a
index (Vec8 bm arr) byte
  | testBit256 bm byte = Just $! V.unsafeIndex arr (sparseIndex bm byte)
  | otherwise = Nothing
{-# INLINE index #-}

------------------------------------------------------------------------
-- Trie8

-- | Split entries based on first byte in the key.
-- Returns a plain 256-element vector (used only during trie
-- construction).
partitionBy :: [([Word8], a)] -> V.Vector [([Word8], a)]
partitionBy l = V.create $ do
  mv <- VM.replicate 256 []
  let go ([], _d) = pure ()
      go (w:wl, d) = do
        el <- VM.read mv (word8ToInt w)
        VM.write mv (word8ToInt w) ((wl, d) : el)
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
mkTrie mkLeaf l
  | all done l = Leaf (mkLeaf (snd <$> l))
  | otherwise =
    let parts = partitionBy l
        bm = buildBitmap (\w -> not (null (parts V.! word8ToInt w)))
        arr = V.map (mkTrie mkLeaf) (V.filter (not . null) parts)
    in Branch (Vec8 bm arr)
  where
    done :: ([Word8], a) -> Bool
    done (remaining, _) = null remaining
