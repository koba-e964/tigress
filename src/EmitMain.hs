module Main where

import LLVM.General.Module

import Data.List (foldl')
import Control.Monad.Except
import Control.Monad.State (StateT, evalStateT, runStateT)
import System.IO
import System.Environment (getArgs)
import System.Console.GetOpt

import Codegen
import Emit
import JIT
import TigressLexer as TL
import TigressParser as TP
import TigressExpr (Expr)


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
    [ Option "o" ["output"]
        (ReqArg (\s conf -> conf { outFile = Just s }) "OUTFILE") "output file name"
    , Option "h?" ["help"]
        (NoArg (\conf -> conf { helpMode = True })) "verbose mode"
    ]

main :: IO ()
main = do
  args <- getArgs
  let (ops, rest, err) = getOpt Permute options args
  unless (null err) $ error $ "err:" ++ show err 
  let conf = foldl' (.) id ops defaultConf -- currently not used
  case rest of
    [] -> evalStateT repl conf
    (_name : _) -> error "TODO:source file"
repl :: StateT Config IO ()
repl = do
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ putStr "> "
    line <- liftIO $ getLine
    let toks = TL.alexScanTokens line
    let exprOrErr = TP.tparse toks
    case exprOrErr of
      Left err -> liftIO (print err)
      Right expr -> do
        liftIO $ do
          x <- runExceptT $ interpretExpr expr
          case x of
            Left err -> hPutStrLn stderr err
            Right _ -> return ()
    repl
  where
    -- compiles expr and displays the LLVM code and the result.
    interpretExpr :: Expr -> ExceptT String IO ()
    interpretExpr expr = do
         liftIO $ print expr
         newmod <- liftEither $ codegen (emptyModule "JITtest") [expr]
         optmod <- optimize newmod (Just 3)
         withContextT $ \ctx -> do
           withModuleFromAST ctx newmod $ \mm -> do
             putStrLn "***** Module before optimization *****"
             s <- moduleLLVMAssembly mm
             putStrLn s
           withModuleFromAST ctx optmod $ \mm -> do
             putStrLn "***** Optimized Module *****"
             s <- moduleLLVMAssembly mm
             putStrLn s
         result <- runJIT optmod
         liftIO $ putStrLn ("result = " ++ show result)

