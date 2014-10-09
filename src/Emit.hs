{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module (moduleAST, withModuleFromAST)

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

import Data.Word (Word)
import Control.Monad ((>=>), forM_, liftM,)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Data.Map as Map
import qualified LLVM.General.AST.IntegerPredicate as IP
import LLVM.General.PassManager (PassSetSpec, defaultCuratedPassSetSpec, optLevel, runPassManager, withPassManager)

import Codegen
import TigressExpr

{- Reference: https://github.com/sdiehl/kaleidoscope -}


codegenTop :: Expr -> LLVM ()

codegenTop (ELet decs exprs) = do
  mapM_ declareTop decs
  codegenTopSub (ELet decs exprs)
codegenTop expr = codegenTopSub expr

codegenTopSub :: Expr -> LLVM ()
codegenTopSub expr = do
  (retty, codegenState) <- liftEither $ runCodegen $ do
      entryBlock <- addBlock entryBlockName
      _ <- setBlock entryBlock
      (ty, val) <- cgen expr
      _ <- case ty of
        AST.VoidType -> retNone
        _            -> ret val
      return ty
  let blks = createBlocks codegenState
  define retty "main" [] blks

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
  ]

cgen :: Expr -> Codegen TypedOperand
cgen (EStr _) = throwError "TODO: string-codegen"
cgen (EInt n) = return (int64, cons $ C.Int 64 n)
cgen ENil     = throwError "TODO: nil"
cgen (ELValue lvalue) = do
  var <- getPtr lvalue
  val <- load var
  return (int64, val)
cgen (EMinus e) = do
  operand <- checkType int64 =<< cgen e
  val <- sub (cons (C.Int 64 0)) operand
  return (int64, val)
cgen (EBin bop e1 e2) =
  case bop of
    BAnd -> sand (cgen e1) (cgen e2)
    BOr  -> sor  (cgen e1) (cgen e2)
    _    ->
      case Map.lookup bop binops of
        Just op -> do
          c1 <- cgen e1 >>= checkType int64
          c2 <- cgen e2 >>= checkType int64
          val <- op c1 c2
          return (int64, val)
        Nothing -> throwError "invalid operator"
cgen (EAsgn lvalue expr) = do
  ptr <- getPtr lvalue
  val <- cgen expr >>= checkType int64
  _ <- store ptr val
  return voidValue
cgen (EApp (Id name) args) = do
  valArgs <- mapM (cgen >=> checkType int64) args
  call (externf (AST.Name name)) valArgs
cgen (ESeq exprs) = cgenSeq exprs
cgen (ERec {}) = throwError "TODO: codegen-record"
cgen (EArr {}) = throwError "TODO: codegen-array"
cgen (EIf cond expr) = do
  ifthen <- addBlock "if.then"
  ifexit <- addBlock "if.exit"
  -- branch
  ccond <- cgen cond >>= checkType int64
  test <- cmp IP.EQ ccond (cons $ C.Int 64 0)
  _ <- cbr test ifexit ifthen -- Branch based on the condition
  -- if.then
  ------------------
  setBlock ifthen
  _ <- cgen expr       -- Generate code for the true branch. Values are discarded.
  _ <- br ifexit                -- Branch to the merge block
  -- if.exit
  ------------------
  setBlock ifexit
  return voidValue

cgen (EIfElse cond etr efl) = conditional (cgen cond) (cgen etr) (cgen efl)
cgen (EWhile cond body) = do
  whileCond  <- addBlock "while.cond"
  whileBegin <- addBlock "while.begin"
  whileExit  <- addBlock "while.exit"
  _ <- br whileCond
  -- while.cond
  setBlock whileCond
  ccond <- cgen cond >>= checkType int64
  pushLoopExit whileExit
  _ <- cbr ccond whileBegin whileExit
  -- while.begin
  setBlock whileBegin
  _ <- cgen body
  _ <- br whileCond
  -- while.exit
  setBlock whileExit
  _ <- popLoopExit
  return voidValue

cgen (EFor (Id name) begin end body) = do
  forCond  <- addBlock "for.cond"
  forBegin <- addBlock "for.begin"
  forExit  <- addBlock "for.exit"
  cend <- cgen end >>= checkType int64 -- Evaluation order of 'end' and 'begin' is not specified. In this implementation, 'end' is evaluated first.
  var <- alloca int64
  assign name var
  cbeg <- cgen begin >>= checkType int64 -- 'begin' is evaluated second.
  _ <- store var cbeg
  pushLoopExit forExit
  _ <- br forCond
  -- for.cond
  setBlock forCond
  curVal <- load var
  ccond <- le curVal cend
  _ <- cbr ccond forBegin forExit
  -- for.begin
  setBlock forBegin
  _ <- cgen body
  curValBody <- load var
  newCurVal <- add curValBody (cons (C.Int 64 1))
  _ <- store var newCurVal
  _ <- br forCond
  -- for.exit
  setBlock forExit
  _ <- popLoopExit
  return voidValue

cgen EBreak = do
  breaker <- addBlock "breaker"
  dummy   <- addBlock "break.dummy"
  lExit <- popLoopExit
  case lExit of
    Nothing    -> throwError "break not in loop"
    Just block -> do
      _ <- br breaker
      setBlock breaker
      _ <- br block
      setBlock dummy
      return voidValue

cgen (ELet decs exprs) = do
  mapM_ declare decs
  cgenSeq exprs

cgenSeq :: [Expr] -> Codegen TypedOperand
cgenSeq [] = return voidValue
cgenSeq ls = liftM last $ mapM cgen ls

-- gets the pointer of lvalue
getPtr :: LValue -> Codegen AST.Operand
getPtr (LId (Id name)) = getvar name
getPtr (LMem {}) = throwError "TODO: member-lvalue-codegen"
getPtr (LIdx {}) = throwError "TODO: index-lvalue-codegen"


-- declaration
declare :: Dec -> Codegen ()
declareTop :: Dec -> LLVM ()

declare (DType _) = return ()
declare (DVar (VarDec (Id name) _mtypeid expr)) = do
  var <- alloca int64
  assign name var
  val <- cgen expr >>= checkType int64
  _ <- store var val
  return ()
declare _ = return ()


declareTop (DFun (FunDec (Id name) typefields _rettypeid body)) = do
  bls <- liftEither $ fmap createBlocks $ execCodegen $ do
      entryBlock <- addBlock entryBlockName
      setBlock entryBlock
      forM_ typefields $ \(TypeField (Id x) _argtypeid) -> do
        var <- alloca int64
        _ <- store var (local (AST.Name x))
        assign x var
      (ty, val) <- cgen body
      case ty of
        AST.VoidType -> retNone
        _            -> ret val
  define int64 name largs bls
  where
    largs = map (\(TypeField (Id x) _typeid) -> (int64, AST.Name x)) typefields
  
declareTop _ = return ()

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

-- | Compiles 'fns' in the module 'mod' and returns new module.
codegen :: AST.Module -> [Expr] -> Either String AST.Module
codegen astmod fns = newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM astmod modn


-- | Optimizes a module. Optimization level is specified in parameter 'opt'.
optimize :: AST.Module -> Maybe Word -> ExceptT String IO AST.Module
optimize astmod opt =
  withContextT $ \context ->
    withModuleFromAST context astmod $ \m ->
      withPassManager (passes opt) $ \pm -> do
        -- Optimization Pass
        _ <- runPassManager pm m
        moduleAST m

-- | Returns passes whose level is 'opt'.
passes :: Maybe Word -> PassSetSpec
passes opt = defaultCuratedPassSetSpec { optLevel = opt }


