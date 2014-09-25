module Main where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map
import qualified LLVM.General.AST.IntegerPredicate as IP
import System.IO

import Codegen
import TigressExpr
import Emit
import TigressLexer as TL
import TigressParser as TP

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
       Left err -> void $ print err
       Right expr -> do
         print expr
         _newmod <- codegen (emptyModule "JITtest") [expr]
         repl

