{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

import Data.Word (Word)
import Control.Monad.Except
import qualified Data.Map as Map
import qualified LLVM.General.AST.IntegerPredicate as IP
import LLVM.General.PassManager

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
  define retty "main" [] blks
  where
    (retty, codegenState) = runCodegen $ do
      entryBlock <- addBlock entryBlockName
      _ <- setBlock entryBlock
      (ty, val) <- cgen expr
      _ <- case ty of
        AST.VoidType -> retNone
        _            -> ret val
      return ty
    blks = createBlocks codegenState

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

checkType :: AST.Type -> (AST.Type, AST.Operand) -> Codegen AST.Operand
checkType ty (realTy, realVal) = do
  when (ty /= realTy) (throwError $ "Type error (expected: " ++ show ty ++ ", actual: " ++ show realTy ++ ")")
  return realVal

voidValue :: (AST.Type, AST.Operand)
voidValue = (AST.VoidType, cons $ C.Undef AST.VoidType)

cgen :: Expr -> Codegen (AST.Type, AST.Operand)
cgen (EStr _) = error "TODO: string-codegen"
cgen (EInt n) = return $ (int64, cons $ C.Int 64 n)
cgen ENil     = error "TODO: nil"
cgen (ELValue lvalue) = do
  var <- getPtr lvalue
  val <- load var
  return (int64, val)
cgen (EMinus e) = do
  operand <- checkType int64 =<< cgen e
  val <- sub (cons (C.Int 64 0)) operand
  return (int64, val)
cgen (EBin bop e1 e2) = do
  case Map.lookup bop binops of
    Just op -> do
      c1 <- cgen e1 >>= checkType int64
      c2 <- cgen e2 >>= checkType int64
      val <- op c1 c2
      return (int64, val)
    Nothing -> error "invalid operator"
cgen (EAsgn lvalue expr) = do
  ptr <- getPtr lvalue
  val <- cgen expr >>= checkType int64
  _ <- store ptr val
  return voidValue
cgen (EApp (Id name) args) = do
  valArgs <- mapM (cgen >=> checkType int64) args
  call (externf (AST.Name name)) valArgs
cgen (ESeq exprs) = cgenSeq exprs
cgen (ERec {}) = error "TODO: codegen-record"
cgen (EArr {}) = error "TODO: codegen-array"
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

cgen (EIfElse cond etr efl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  -- branch
  ccond <- cgen cond >>= checkType int64
  test <- cmp IP.EQ ccond (cons $ C.Int 64 0)
  _ <- cbr test ifelse ifthen -- Branch based on the condition
  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen etr       -- Generate code for the true branch
  _ <- br ifexit                -- Branch to the merge block
  ifthenEnd <- getBlock
  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen efl       -- Generate code for the true branch
  _ <- br ifexit                -- Branch to the merge block
  ifelseEnd <- getBlock
  -- if.exit
  ------------------
  setBlock ifexit
  phi [(trval, ifthenEnd), (flval, ifelseEnd)]

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
    Nothing    -> error "break not in loop"
    Just block -> do
      _ <- br breaker
      setBlock breaker
      _ <- br block
      setBlock dummy
      return voidValue

cgen (ELet decs exprs) = do
  mapM_ declare decs
  cgenSeq exprs

cgenSeq :: [Expr] -> Codegen (AST.Type, AST.Operand)
cgenSeq [] = return voidValue
cgenSeq ls = liftM last $ mapM cgen ls

-- gets the pointer of lvalue
getPtr :: LValue -> Codegen AST.Operand
getPtr (LId (Id name)) = getvar name
getPtr (LMem {}) = error "TODO: member-lvalue-codegen"
getPtr (LIdx {}) = error "TODO: index-lvalue-codegen"


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


declareTop (DFun (FunDec (Id name) typefields _rettypeid body)) =
  define int64 name largs bls
  where
    largs = map (\(TypeField (Id x) _typeid) -> (int64, AST.Name x)) typefields
    bls = createBlocks $ execCodegen $ do
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
  
declareTop _ = return ()

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

-- | Compiles 'fns' in the module 'mod' and returns new module.
-- | This prints the unoptimized module, but returns the optimized module.
codegen :: AST.Module -> [Expr] -> IO AST.Module
codegen astmod fns = return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM astmod modn

-- | Optimizes a module. Optimization level is specified in parameter 'opt'.
optimize :: AST.Module -> Maybe Word -> IO AST.Module
optimize astmod opt = (either fail return =<<) $
  withContext $ \context -> do
    runExceptT $ withModuleFromAST context astmod $ \m -> do
      withPassManager (passes opt) $ \pm -> do
        -- Optimization Pass
        _ <- runPassManager pm m
        moduleAST m

-- | Returns passes whose level is 'opt'.
passes :: Maybe Word -> PassSetSpec
passes opt = defaultCuratedPassSetSpec { optLevel = opt }


