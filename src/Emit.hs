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
cgen (ELValue lvalue) = do
  var <- getPtr lvalue
  load var
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
cgen (EAsgn lvalue expr) = do
  ptr <- getPtr lvalue
  val <- cgen expr
  store ptr val
  return $ cons $ C.Undef AST.VoidType
cgen (ESeq exprs) = cgenSeq exprs
cgen (EIf cond expr) = do
  ifthen <- addBlock "if.then"
  ifexit <- addBlock "if.exit"
  -- branch
  ccond <- cgen cond
  test <- cmp IP.EQ ccond (cons $ C.Int 64 0)
  cbr test ifexit ifthen -- Branch based on the condition
  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen expr       -- Generate code for the true branch
  br ifexit                -- Branch to the merge block
  -- if.exit
  ------------------
  setBlock ifexit
  return $ cons $ C.Undef AST.VoidType

cgen (EIfElse cond etr efl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  -- branch
  ccond <- cgen cond
  test <- cmp IP.EQ ccond (cons $ C.Int 64 0)
  cbr test ifelse ifthen -- Branch based on the condition
  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen etr       -- Generate code for the true branch
  br ifexit                -- Branch to the merge block
  ifthenEnd <- getBlock
  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen efl       -- Generate code for the true branch
  br ifexit                -- Branch to the merge block
  ifelseEnd <- getBlock
  -- if.exit
  ------------------
  setBlock ifexit
  phi int64 [(trval, ifthenEnd), (flval, ifelseEnd)]


cgen (ELet decs exprs) = do
  mapM_ declare decs
  cgenSeq exprs

cgenSeq :: [Expr] -> Codegen AST.Operand
cgenSeq [] = return $ cons $ C.Undef AST.VoidType
cgenSeq ls = liftM last $ mapM cgen ls

-- gets the pointer of lvalue
getPtr :: LValue -> Codegen AST.Operand
getPtr (LId (Id name)) = getvar name

-- declaration
declare :: Dec -> Codegen ()
declare (DType _) = return ()
declare (DVar (VarDec (Id name) _mtypeid expr)) = do
  var <- alloca int64
  assign name var
  val <- cgen expr
  store var val
  return ()
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

