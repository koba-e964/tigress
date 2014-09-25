{-# LANGUAGE OverloadedStrings #-}

module Emit where

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

import Codegen
import TigressExpr

{- Reference: https://github.com/sdiehl/kaleidoscope -}

codegenTop :: Expr -> LLVM ()
codegenTop expr = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      _ <- setBlock entry
      cgen expr >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

eq :: AST.Operand -> AST.Operand -> Codegen AST.Operand
neq :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
gt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
le :: AST.Operand -> AST.Operand -> Codegen AST.Operand
ge :: AST.Operand -> AST.Operand -> Codegen AST.Operand

eq = cmp IP.EQ
neq = cmp IP.NE
lt = cmp IP.SLT
gt = cmp IP.SGT
le = cmp IP.SLE
ge = cmp IP.SGE

binops :: Map.Map BinOp (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binops = Map.fromList [
      (BAdd, add)
    , (BSub, sub)
    , (BMul, mul)
    , (BDiv, Codegen.div)
    , (BEq, eq)
    , (BNeq, neq)
    , (BLt, lt)
    , (BGt, gt)
    , (BLe, le)
    , (BGe, ge)
    , (BAnd, undefined)
    , (BOr, undefined)
  ]

cgen :: Expr -> Codegen AST.Operand
cgen (EInt n) = return $ cons $ C.Int 64 n
cgen (EMinus e) = do
  operand <- cgen e
  sub (cons (C.Int 64 0)) operand
cgen (EBin bop e1 e2) = do
  case Map.lookup bop binops of
    Just op -> do
      c1 <- cgen e1
      c2 <- cgen e2
      op c1 c2
    Nothing -> error "invalid operator"

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
