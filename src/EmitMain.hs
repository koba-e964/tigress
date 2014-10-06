module Main where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Data.List (foldl')
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map
import qualified LLVM.General.AST.IntegerPredicate as IP
import System.IO
import System.Environment (getArgs)
import System.Console.GetOpt

import Codegen
import TigressExpr
import Emit
import JIT
import TigressLexer as TL
import TigressParser as TP

data Config = Config
  { outFile :: Maybe FilePath
  , helpMode :: Bool
  } deriving Show

defaultConf :: Config
defaultConf = Config
  { outFile = Nothing
  , helpMode = False
  }

options :: [OptDescr (Config -> Config)]
options =
    [ Option ['o'] ["output"]
        (ReqArg (\s conf -> conf { outFile = Just s }) "OUTFILE") "output file name"
    , Option ['h','?'] ["help"]
        (NoArg (\conf -> conf { helpMode = True })) "verbose mode"
    ]

main :: IO ()
main = do
  args <- getArgs
  let (ops, rest, err) = getOpt Permute options args
  when (not (null err)) $ error $ "err:" ++ show err 
  let conf = foldl' (.) id ops defaultConf
  case rest of
    [] -> repl
    (name : _) -> error "TODO:source file"
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
         newmod <- codegen (emptyModule "JITtest") [expr]
         optmod <- optimize newmod (Just 3)
         withContext $ \ctx -> do
           runExceptT $ withModuleFromAST ctx newmod $ \mm -> do
             putStrLn "***** Module before optimization *****"
             s <- moduleLLVMAssembly mm
             putStrLn s
           runExceptT $ withModuleFromAST ctx optmod $ \mm -> do
             putStrLn "***** Optimized Module *****"
             s <- moduleLLVMAssembly mm
             putStrLn s
         runJIT optmod >>= either fail (\x -> putStrLn ("result = " ++ show x))
         repl

