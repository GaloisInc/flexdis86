{- |
Module      :  $Header$
Copyright   :  (c) Galois, Inc 2018
Maintainer  :  jhendrix@galois.com

This declares types needed to support relocations.
-}
module Data.BinarySymbols
  ( SymbolIdentifier(..)
  , SymbolVersion(..)
  , SectionIndex
  , SegmentIndex
  , SymbolName
  ) where

import qualified Data.ByteString.Char8 as BSC
import           Data.Word

-- | Sections are regions of an executable, object, or shared library
-- file.
--
-- A section index is a number that should uniquely identify a section
-- within such a file.
type SectionIndex = Word16

-- | The index of a memory segment.
type SegmentIndex = Word16

type SymbolName = BSC.ByteString

-- | Characterized the version information on a symbol
data SymbolVersion
   = UnversionedSymbol
     -- ^ The symbol had no or the default *global* version information.
   | ObjectSymbol
     -- ^ The symbol comes from an object file and hence does not
     -- have GNU version information.  Version information
     -- may be part of the symbol name however.
   | VersionedSymbol !BSC.ByteString !BSC.ByteString
     -- ^ A symbol with version information from version information
     -- in a shared library or executable.
     --
     -- The first value is the name of the shared object.  The second
     -- is the version associated with the symbol.
  deriving (Eq, Ord, Show)

-- | An identifier that represents some offset in a binary.
data SymbolIdentifier
   = SymbolRelocation !SymbolName !SymbolVersion
     -- ^ Denotes the address of the symbol that matches the name and
     -- version constraints.
   | SectionIdentifier !SectionIndex
     -- ^ Denotes the address of the section with the given index.
   | SegmentBaseAddr !SegmentIndex
     -- ^ This denotes the address of the segment with the given
     -- index.
   | LoadBaseAddr
     -- ^ This denotes the base load address of a shared executable.
  deriving (Eq, Ord)

instance Show SymbolIdentifier where
  showsPrec _ (SymbolRelocation nm ver) =
    case ver of
      UnversionedSymbol -> showString (BSC.unpack nm)
      ObjectSymbol -> showString (BSC.unpack nm)
      VersionedSymbol symName soName ->
        showString (BSC.unpack nm)
        . showChar '@' . showString (BSC.unpack symName)
        . showChar '(' . showString (BSC.unpack soName) . showChar ')'
  showsPrec _ (SectionIdentifier idx) =
    showString "section_" . shows idx
  showsPrec _ (SegmentBaseAddr idx) =
    showString "segment_" . shows idx
