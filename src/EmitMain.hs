module Main where

import LLVM.General.Module
import LLVM.General.Context

import Data.List (foldl')
import Control.Monad.Except
import System.IO
import System.Environment (getArgs)
import System.Console.GetOpt

import Codegen
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
  let _conf = foldl' (.) id ops defaultConf -- currently not used
  case rest of
    [] -> repl
    (_name : _) -> error "TODO:source file"
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
         _ <- withContext $ \ctx -> do
           _ <- runExceptT $ withModuleFromAST ctx newmod $ \mm -> do
             putStrLn "***** Module before optimization *****"
             s <- moduleLLVMAssembly mm
             putStrLn s
           runExceptT $ withModuleFromAST ctx optmod $ \mm -> do
             putStrLn "***** Optimized Module *****"
             s <- moduleLLVMAssembly mm
             putStrLn s
         runJIT optmod >>= either fail (\x -> putStrLn ("result = " ++ show x))
         repl

