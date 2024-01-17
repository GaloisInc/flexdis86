{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad (when)
import           Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import qualified Control.Monad.Fail as Fail
import           Control.Monad.State (MonadState(..), State, evalState)
import qualified Data.ByteString as BS
import           Data.Word (Word8)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec (lift)
import           Text.Read (Read(..), readMaybe, readListPrecDefault)
import           Text.Read.Lex (readHexP)

import           Flexdis86

newtype SimpleByteReader a = SBR { unSBR :: ExceptT String (State BS.ByteString) a }
  deriving (Applicative, Functor)

instance ByteReader SimpleByteReader where
  readByte = do
    bs <- SBR get
    case BS.uncons bs of
      Nothing       -> SBR (throwError "No more bytes")
      Just (b, bs') -> SBR (put bs') >> return b

instance Monad SimpleByteReader where
#if !(MIN_VERSION_base(4,11,0))
  return v      = SBR (return v)
#endif
  (SBR v) >>= f = SBR $ v >>= unSBR . f
#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif


instance Fail.MonadFail SimpleByteReader where
  fail s        = SBR $ throwError s

-- | A wrapper around 'Word8' whose 'Read' instance is parsed as a hexadecimal
-- number, not a decimal one.
newtype Hex = Hex { unHex :: Word8 } deriving (Eq, Num)

instance Read Hex where
  readPrec = ReadPrec.lift readHexP
  readListPrec = readListPrecDefault

runSimpleByteReader :: SimpleByteReader a -> BS.ByteString -> Either String a
runSimpleByteReader (SBR m) s = evalState (runExceptT m) s

usageExit :: IO a
usageExit = do putStrLn "DumpInstr aa bb cc dd ee ff ..."
               exitFailure

main :: IO ()
main = do args <- getArgs
          when (args == []) usageExit

          let maybeNums = map readMaybe args

          nums <-
            case sequence maybeNums of
              Just nums -> return nums
              Nothing   -> usageExit

          let bs = BS.pack $ map unHex nums

          case runSimpleByteReader disassembleInstruction bs of
           Right ii -> print ii >> print (ppInstruction ii)
           Left e   -> error ("No parse: " ++ e)
