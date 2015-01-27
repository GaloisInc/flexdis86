
module Main where

import           Control.Monad (when)
import qualified Data.ByteString as BS
import           Data.Maybe (catMaybes)
import           Numeric (readHex)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

import           Flexdis86

usageExit :: IO ()
usageExit = do putStrLn "DumpInstr aa bb cc dd ee ff ..."
               exitFailure

main :: IO ()
main = do args <- getArgs
          when (args == []) usageExit

          let nums = map readHex args
              
          when (any (\v -> length v /= 1 || any ((/=) 0 . length . snd) v) nums) usageExit

          let bs = BS.pack $ map (fst . head) nums

          mapM_ print (catMaybes $ map disInstruction $ disassembleBuffer defaultX64Disassembler bs)
