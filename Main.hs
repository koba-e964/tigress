{- The main module for compiler for Tigress, a subset of Tiger. 
 Reference : http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf -}
module Main where

import TigressToken
import TigressLexer as TL
import TigressParser as TP

str :: String
str = "let var N := 8 " ++ 
      "type intArray = array of int " ++
      "var row := intArray [ N ] of 0 " ++
      "var col := intArray [ N ] of 0 " ++
      "var diag1 := intArray [ N+N-1 ] of 0 " ++
      "in try(0) end"



main :: IO ()
main = do
    let toks = TL.alexScanTokens str
    print toks
    print $ TP.tparse toks

