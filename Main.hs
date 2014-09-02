{- The main module for compiler for Tigress, a subset of Tiger. -}
module Main where

import TigressToken
import TigressLexer as TL

str :: String
str = "var N := 8 " ++ 
      "type intArray = array of int " ++
      "var row := intArray [ N ] of 0 " ++
      "var col := intArray [ N ] of 0 " ++
      "var diag1 := intArray [ N+N-1 ] of 0 "



main :: IO ()
main = do
    let toks = TL.alexScanTokens str
    print toks

