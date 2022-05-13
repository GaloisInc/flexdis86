-- | TODO RGS: Docs
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
module Flexdis86.DefaultParser.TH
  ( optableData
  ) where

import           Control.Monad (when)
import qualified Data.ByteString as BS
import           Data.ByteString.Unsafe (unsafePackAddressLen)
import           Language.Haskell.TH.Syntax
import qualified System.Directory as D
import qualified System.FilePath as F
import           System.IO.Unsafe (unsafePerformIO)

-- | A bytestring containing the compiled XML optable specification.
--
-- TODO RGS: Docs
optableData :: BS.ByteString
optableData =
 -- The @getPathToOptableXML@ computes an absolute path to the XML
 -- file. This is helpful in case our current working directory is not
 -- the directory containing the @flexdis86.cabal@ file. This happens,
 -- e.g., when we build flexdis86 as a dependency of another package
 -- in Emacs @haskell-mode@.
 ($(do let getPathToOptableXML :: IO FilePath
           getPathToOptableXML = do
             let relativePath = "data/optable.xml"
             absPathToThisFile <- D.makeAbsolute __FILE__
             let absolutePath =
                   -- Remove 'src/Flexdis86/DefaultParser/TH.hs' and add
                   -- 'data/optable.xml'.
                   (F.takeDirectory . F.takeDirectory . F.takeDirectory . F.takeDirectory $
                    absPathToThisFile) F.</> relativePath
             exists <- D.doesFileExist absolutePath
             when (not exists) $
               error $ "Can't find \"data/optable.xml\"! Tried " ++
                       absolutePath
             return absolutePath

       path <- qRunIO getPathToOptableXML
       qAddDependentFile path
       contents <- qRunIO $ BS.readFile path
       let blen :: Int
           blen = fromIntegral (BS.length contents)
#if MIN_VERSION_template_haskell(2,8,0)
       let addr = LitE $ StringPrimL $ BS.unpack contents
#else
       let addr = LitE $ StringPrimL $ UTF8.toString contents
#endif
       [| unsafePerformIO $ unsafePackAddressLen blen $(return addr) |]))
