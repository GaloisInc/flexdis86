{- |
Module      :  $Header$
Copyright   :  (c) Galois, Inc 2013-2026
Maintainer  :  langston@galois.com

Defines the default parser from optable.xml as compiled in.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
module Flexdis86.DefaultParser
  ( defaultX64Disassembler
  , defaultX64Assembler
  ) where

import           Control.Monad (replicateM, when)
import qualified Data.Binary as Bin
import           Data.Binary.Get (Get, runGet)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector as V
import           Data.Word (Word32, Word8)
import           Language.Haskell.TH.Syntax (Exp(..), Lit(..), qAddDependentFile, qRunIO)
import qualified System.Directory as D
import qualified System.FilePath as F
import           System.IO.Unsafe (unsafePerformIO) -- for optableBlobs

import           Flexdis86.Assembler
import           Flexdis86.Disassembler
import           Flexdis86.OpTable (Def)
import           Flexdis86.OpTable.Parse (parseOpTable)
import           Flexdis86.Prefixes (VEX(..))

-- | Both compile-time blobs embedded as @Addr#@ string literals
-- ('StringPrimL'), computed from @optable.xml@ in a single TH splice so
-- the XML is only parsed once at compile time.
--
-- * @fst@: the def blob — @nDefs :: Word32@ + @nDefs@ 'Binary'-encoded
--   'Def' values, sorted by trie key.  Used by the assembler and as the
--   Def-index lookup table when decoding the pairs blob.
--
-- * @snd@: the pairs blob — @nPairs :: Word32@ + @nPairs@
--   @(keyLen :: Word8, key :: [Word8], idx :: Word32)@ entries, sorted by
--   key.  Used once at disassembler startup, then GC\'d.
--
-- Keeping the two blobs separate means the decoded pairs (which are large
-- at runtime) need not be kept live alongside the @['Def']@ list that the
-- assembler holds.
--
-- This is Pareto-optimal among compile-time embedding strategies:
--
-- * Embedding the XML file: requires otherwise-unnecessary runtime XML parsing
-- * Running @mkOpcodeTable@ in TH: OOMs due to construction of huge trie
-- * @Lift@ing the @Def@s: huge artifacts due to one relocation per ADT field
optableBlobs :: (BS.ByteString, BS.ByteString)
{-# NOINLINE optableBlobs #-}
optableBlobs =
 -- The @getPathToOptableXML@ computes an absolute path to the XML
 -- file. This is helpful in case our current working directory is not
 -- the directory containing the @flexdis86.cabal@ file. This happens,
 -- e.g., when we build flexdis86 as a dependency of another package
 -- in Emacs @haskell-mode@.
 $(do let getPathToOptableXML :: IO FilePath
          getPathToOptableXML = do
            let relativePath = "data/optable.xml"
            absPathToThisFile <- D.makeAbsolute __FILE__
            let absolutePath =
                  -- Remove 'src/Flexdis86/DefaultParser.hs' and add
                  -- 'data/optable.xml'.
                  (F.takeDirectory . F.takeDirectory . F.takeDirectory $
                   absPathToThisFile) F.</> relativePath
            exists <- D.doesFileExist absolutePath
            when (not exists) $
              error $ "Can't find \"data/optable.xml\"! Tried " ++
                      absolutePath
            return absolutePath

      path <- qRunIO getPathToOptableXML
      qAddDependentFile path
      contents <- qRunIO $ BS.readFile path
      (defBlob, pairBlob) <-
        case parseOpTable contents of
          Left  e -> fail ("optableBlobs: failed to parse optable.xml: " ++ e)
          Right p -> return p
      -- Emit: unsafePerformIO (BSU.unsafePackAddressLen n "\xNN..."#)
      -- StringPrimL puts the bytes in .rodata as a single Addr# literal —
      -- one blob, zero relocations.
      let mkBlobExp blob =
            let n  = LBS.length blob
                ws = LBS.unpack blob
            in AppE (VarE 'unsafePerformIO) $
               AppE (AppE (VarE 'BSU.unsafePackAddressLen)
                          (LitE (IntegerL (fromIntegral n))))
                    (LitE (StringPrimL ws))
      return $ TupE [Just (mkBlobExp defBlob), Just (mkBlobExp pairBlob)])

-- | Instruction definitions decoded from the def blob, for the assembler.
optableDefs :: [Def]
{-# NOINLINE optableDefs #-}
optableDefs = runGet getBlob (LBS.fromStrict (fst optableBlobs))
  where
    getBlob :: Get [Def]
    getBlob = do
      nDefs <- Bin.get :: Get Word32
      replicateM (fromIntegral nDefs) Bin.get

defaultX64Disassembler :: NextOpcodeTable
{-# NOINLINE defaultX64Disassembler #-}
defaultX64Disassembler =
    case mkX64DisassemblerFromExpanded pairs of
      Right v -> v
      Left  s -> error ("defaultX64Disassembler: " ++ s)
  where
    -- Local binding: not a CAF, so the decoded list is GC'd once the trie
    -- thunk is reduced to its result.
    pairs :: [([Word8], (Maybe VEX, Def))]
    pairs = runGet getPairs (LBS.fromStrict (snd optableBlobs))

    defVec :: V.Vector Def
    defVec = V.fromList optableDefs

    getPairs :: Get [([Word8], (Maybe VEX, Def))]
    getPairs = do
      nPairs <- Bin.get :: Get Word32
      replicateM (fromIntegral nPairs) getPair

    getPair :: Get ([Word8], (Maybe VEX, Def))
    getPair = do
      keyLen <- Bin.get :: Get Word8
      key    <- replicateM (fromIntegral keyLen) Bin.get
      idx    <- Bin.get :: Get Word32
      let d = defVec V.! fromIntegral idx
      return (key, (vexFromKey key, d))

    vexFromKey :: [Word8] -> Maybe VEX
    vexFromKey (0xC5 : b       : _) = Just (VEX2 b)
    vexFromKey (0xC4 : b1 : b2 : _) = Just (VEX3 b1 b2)
    vexFromKey _                    = Nothing

defaultX64Assembler :: AssemblerContext
defaultX64Assembler = mkX64Assembler optableDefs
