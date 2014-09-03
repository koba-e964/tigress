{- The main module for compiler for Tigress, a subset of Tiger. 
 Reference : http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf -}
module Main where

import TigressToken
import TigressLexer as TL
import TigressParser as TP
import TigressEval as TE
import System.IO

str :: String
str = "let var N := 8 " ++ 
      "type intArray = array of int " ++
      "var row := intArray [ N ] of 0 " ++
      "var col := intArray [ N ] of 0 " ++
      "var diag1 := intArray [ N+N-1 ] of 0 " ++
      "in try(0) end"



main :: IO ()
main = repl

repl :: IO ()
repl = do
    hSetBuffering stdout NoBuffering
    putStr "> "
    line <- getLine
    let toks = TL.alexScanTokens line
    let exprOrErr = TP.tparse toks
    case exprOrErr of
       Left err -> print err >> return ()
       Right expr -> do
        print expr
        res <- runTigress $ TE.eval expr
        case res of
            Left err  -> print err >> return ()
            Right val -> do
                putStrLn $ "- : ???\n" ++ show val
                repl

