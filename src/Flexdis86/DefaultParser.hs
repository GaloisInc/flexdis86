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
import           System.IO.Unsafe (unsafePerformIO)

import           Flexdis86.Assembler
import           Flexdis86.Disassembler
import           Flexdis86.OpTable (Def)
import           Flexdis86.OpTable.Parse (parseOpTable)
import           Flexdis86.Prefixes (VEX(..))

-- | The instruction definitions parsed from @optable.xml@ at compile time,
-- serialized via 'Data.Binary' and embedded as a single @Addr#@ string
-- literal ('StringPrimL').
--
-- This is Pareto-optimal among compile-time embedding strategies, when
-- considering trade-offs including compile time, compiler memory usage,
-- runtime, and artifact size. Alternatives considered:
--
-- * Embedding the XML file: requires otherwise-unnecessary runtime XML parsing
-- * Running @mkOpcodeTable@ in TH: OOMs due to construction of huge trie
-- * @Lift@ing the @Def@s: huge artifacts due to one relocation per ADT field
optableBytes :: BS.ByteString
{-# NOINLINE optableBytes #-}
optableBytes =
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
      encoded <- case parseOpTable contents of
                   Left  e -> fail ("optableBytes: failed to parse optable.xml: " ++ e)
                   Right b -> return b
      let n  = LBS.length encoded
          ws = LBS.unpack encoded
      -- Emit: unsafePerformIO (BSU.unsafePackAddressLen n "\xNN..."#)
      -- StringPrimL puts the bytes in .rodata as a single Addr# literal —
      -- one blob, zero relocations.
      return $
        AppE (VarE 'unsafePerformIO) $
        AppE (AppE (VarE 'BSU.unsafePackAddressLen)
                   (LitE (IntegerL (fromIntegral n))))
             (LitE (StringPrimL ws)))

-- | Both decoded sections of the compile-time blob, evaluated once.
--
-- Section 1: the sorted @['Def']@ array (for the assembler and as a lookup
-- table for expanded pairs).  Section 2: pre-sorted
-- @[('[Word8]', ('Maybe' 'VEX', 'Def'))]@ expanded pairs ready for
-- 'Flexdis86.Disassembler.mkX64DisassemblerFromExpanded'.
optableDecoded :: ([Def], [([Word8], (Maybe VEX, Def))])
{-# NOINLINE optableDecoded #-}
optableDecoded = runGet getBlob (LBS.fromStrict optableBytes)
  where
    getBlob :: Get ([Def], [([Word8], (Maybe VEX, Def))])
    getBlob = do
      nDefs <- Bin.get :: Get Word32
      defs  <- replicateM (fromIntegral nDefs) Bin.get
      let defVec = V.fromList defs
      nPairs <- Bin.get :: Get Word32
      pairs  <- replicateM (fromIntegral nPairs) (getPair defVec)
      return (defs, pairs)

    getPair :: V.Vector Def -> Get ([Word8], (Maybe VEX, Def))
    getPair defVec = do
      keyLen <- Bin.get :: Get Word8
      key    <- replicateM (fromIntegral keyLen) Bin.get
      idx    <- Bin.get :: Get Word32
      let d = defVec V.! fromIntegral idx
      return (key, (vexFromKey key, d))

    vexFromKey :: [Word8] -> Maybe VEX
    vexFromKey (0xC5 : b       : _) = Just (VEX2 b)
    vexFromKey (0xC4 : b1 : b2 : _) = Just (VEX3 b1 b2)
    vexFromKey _                    = Nothing

-- | Instruction definitions (Section 1 of the blob), for the assembler.
optableDefs :: [Def]
optableDefs = fst optableDecoded

-- | Pre-sorted expanded trie entries (Section 2 of the blob), for the
-- disassembler.
optableExpanded :: [([Word8], (Maybe VEX, Def))]
optableExpanded = snd optableDecoded

defaultX64Disassembler :: NextOpcodeTable
defaultX64Disassembler = p
  where p = case mkX64DisassemblerFromExpanded optableExpanded of
              Right v -> v
              Left  s -> error ("defaultX64Disassembler: " ++ s)

defaultX64Assembler :: AssemblerContext
defaultX64Assembler = mkX64Assembler optableDefs
