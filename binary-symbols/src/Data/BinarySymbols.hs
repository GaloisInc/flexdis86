{- |
Module      :  $Header$
Copyright   :  (c) Galois, Inc 2018
Maintainer  :  jhendrix@galois.com

This declares types needed to support relocations.
-}
module Data.BinarySymbols
  ( SymbolIdentifier(..)
  , VersionedSymbol(..)
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
   | ObjectDefaultSymbol !BSC.ByteString
     -- ^ The symbol comes from an object file and thus just
     -- has a version string associated with it (but we need the
     -- map file to know).
     --
     -- Default symbols are the ones used in linking against other objects.
   | ObjectNonDefaultSymbol !BSC.ByteString
     -- ^ The symbol comes from an object file and hence does not
     -- have GNU version information.  Version information
     -- may be part of the symbol name however.
     --
     -- Nondefault symbols will not be used in linking objects, but
     -- may appear in the final library and be linked dynamically.
   | VersionedSymbol !BSC.ByteString !BSC.ByteString
     -- ^ A symbol with version information from version information
     -- in a shared library or executable.
     --
     -- The first value is the name of the shared object.  The second
     -- is the version associated with the symbol.
  deriving (Eq, Ord, Show)

-- | A symbol name along with version information.
data VersionedSymbol = VerSym { versymName :: !BSC.ByteString
                              , versymVersion :: !SymbolVersion
                              }

instance Show VersionedSymbol where
  showsPrec _ (VerSym nm ver) =
    showString (BSC.unpack nm)
      . case ver of
          UnversionedSymbol -> id
          ObjectDefaultSymbol    ver -> showString "@@" . showString (BSC.unpack ver)
          ObjectNonDefaultSymbol ver -> showString  "@" . showString (BSC.unpack ver)
          VersionedSymbol symName soName
            -> showChar '@' . showString (BSC.unpack symName)
             . showChar '(' . showString (BSC.unpack soName) . showChar ')'

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
  showsPrec p (SymbolRelocation nm ver) =
    showsPrec p (VerSym nm ver)
  showsPrec _ (SectionIdentifier idx) =
    showString "section_" . shows idx
  showsPrec _ (SegmentBaseAddr idx) =
    showString "segment_" . shows idx
  showsPrec _ LoadBaseAddr =
    showString "base_addr"
