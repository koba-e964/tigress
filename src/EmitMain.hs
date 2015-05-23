module Main where

import LLVM.General.Module

import Data.List (foldl')
import Control.Monad.Except
import Control.Monad.State (StateT, evalStateT)
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
    [] -> do
      case outFile conf of
        Nothing -> repl
        Just path -> do
          handle <- openFile path WriteMode
          line <- getLine
          liftError $ interpretString line handle False
          hClose handle
    (_name : _) -> error "TODO:source file"
repl :: IO ()
repl = do
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ putStr "> "
    line <- liftIO $ getLine
    liftError $ interpretString line stdout True
    repl
-- compiles expr and displays the LLVM code and the result.
-- if 'isStdOut' is True, the result will be written to the stdout.
-- otherwise, the result will be written to 'handle'.
interpretString :: String -> Handle -> Bool -> ExceptT String IO ()
interpretString str handle isStdOut = do
    let toks = TL.alexScanTokens str
    let exprOrErr = TP.tparse toks
    case exprOrErr of
      Left err -> liftIO (hPutStrLn stderr err)
      Right expr -> do
        liftIO $ do
          x <- runExceptT $ interpretExpr expr handle isStdOut
          case x of
            Left err -> hPutStrLn stderr err
            Right _ -> return ()

interpretExpr :: Expr -> Handle -> Bool -> ExceptT String IO ()
interpretExpr expr handle isStdOut = do
  liftIO $ print expr
  newmod <- liftEither $ codegen (emptyModule "JITtest") [expr]
  optmod <- optimize newmod (Just 3)
  withContextT $ \ctx -> do
   if isStdOut then do
     withModuleFromAST ctx newmod $ \mm -> do
       putStrLn "***** Module before optimization *****"
       s <- moduleLLVMAssembly mm
       putStrLn s
     withModuleFromAST ctx optmod $ \mm -> do
       putStrLn "***** Optimized Module *****"
       s <- moduleLLVMAssembly mm
       putStrLn s
   else
     withModuleFromAST ctx optmod $ \mm -> do
       s <- moduleLLVMAssembly mm
       hPutStr handle s
  result <- runJIT optmod
  liftIO $ putStrLn ("result = " ++ show result)

