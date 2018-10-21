module Main where

import Lib
import qualified Data.ByteString.Lazy as BSL
import System.Environment

help = "Laspell - A preprocessor for Haskell to use LISP syntax.\n"
       ++ "USAGE: laspell ORIGINAL FROM TO\n"
       ++ "  where ORIGINAL is the original filename;\n"
       ++ "        FROM is the file to read from;\n"
       ++ "        TO is the file to write to."

preprocessFile from to = do
  src <- readFile from
  BSL.writeFile to (preprocess from src)

main :: IO ()
main = getArgs >>= (\args -> if length args == 3
                             then preprocessFile (args !! 1) (args !! 2)
                             else putStrLn ("Error: Expects 3 args.\n" ++ help))
