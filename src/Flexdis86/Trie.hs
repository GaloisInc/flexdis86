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
  ) where

import qualified Control.DeepSeq as DS
import           Data.Word (Word8)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
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
mkTrie mkLeaf l
  | all done l = Leaf (mkLeaf (snd <$> l))
  | otherwise =
    let v = partitionBy l
        g i = mkTrie mkLeaf (v `index` i)
        tbl = generate g
    in Branch tbl
  where
    done :: ([Word8], a) -> Bool
    done (remaining, _) = null remaining

