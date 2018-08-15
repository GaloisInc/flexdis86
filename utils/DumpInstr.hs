{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad (when)
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString as BS
import           Data.Maybe (catMaybes)
import           Numeric (readHex)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

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
  return v      = SBR (return v)
  (SBR v) >>= f = SBR $ v >>= unSBR . f
  fail s        = SBR $ throwError s


runSimpleByteReader :: SimpleByteReader a -> BS.ByteString -> Either String a
runSimpleByteReader (SBR m) s = evalState (runExceptT m) s

usageExit :: IO ()
usageExit = do putStrLn "DumpInstr aa bb cc dd ee ff ..."
               exitFailure

main :: IO ()
main = do args <- getArgs
          when (args == []) usageExit

          let nums = map readHex args

          when (any (\v -> length v /= 1 || any ((/=) 0 . length . snd) v) nums) usageExit

          let bs = BS.pack $ map (fst . head) nums

          case runSimpleByteReader disassembleInstruction bs of
           Right ii -> print ii >> print (ppInstruction ii)
           Left e   -> error ("No parse: " ++ e)
