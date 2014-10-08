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
  define int64 "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entryBlock <- addBlock entryBlockName
      _ <- setBlock entryBlock
      cgen expr >>= ret

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
cgen (EStr _) = error "TODO: string-codegen"
cgen (EInt n) = return $ cons $ C.Int 64 n
cgen ENil     = error "TODO: nil"
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
  _ <- store ptr val
  return $ cons $ C.Undef AST.VoidType
cgen (EApp (Id name) args) = do
  valArgs <- mapM cgen args
  call (externf (AST.Name name)) valArgs
cgen (ESeq exprs) = cgenSeq exprs
cgen (ERec {}) = error "TODO: codegen-record"
cgen (EArr {}) = error "TODO: codegen-array"
cgen (EIf cond expr) = do
  ifthen <- addBlock "if.then"
  ifexit <- addBlock "if.exit"
  -- branch
  ccond <- cgen cond
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
  return $ cons $ C.Undef AST.VoidType

cgen (EIfElse cond etr efl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  -- branch
  ccond <- cgen cond
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
  phi int64 [(trval, ifthenEnd), (flval, ifelseEnd)]

cgen (EWhile cond body) = do
  whileCond  <- addBlock "while.cond"
  whileBegin <- addBlock "while.begin"
  whileExit  <- addBlock "while.exit"
  _ <- br whileCond
  -- while.cond
  setBlock whileCond
  ccond <- cgen cond
  pushLoopExit whileExit
  _ <- cbr ccond whileBegin whileExit
  -- while.begin
  setBlock whileBegin
  _ <- cgen body
  _ <- br whileCond
  -- while.exit
  setBlock whileExit
  _ <- popLoopExit
  return $ cons $ C.Undef AST.VoidType

cgen (EFor (Id name) begin end body) = do
  forCond  <- addBlock "for.cond"
  forBegin <- addBlock "for.begin"
  forExit  <- addBlock "for.exit"
  cend <- cgen end -- Evaluation order of 'end' and 'begin' is not specified. In this implementation, 'end' is evaluated first.
  var <- alloca int64
  assign name var
  cbeg <- cgen begin -- 'begin' is evaluated second.
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
  return $ cons $ C.Undef AST.VoidType -- TODO: type error occurs if this value is regarded as int64.

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
      return $ cons $ C.Undef AST.VoidType

cgen (ELet decs exprs) = do
  mapM_ declare decs
  cgenSeq exprs

cgenSeq :: [Expr] -> Codegen AST.Operand
cgenSeq [] = return $ cons $ C.Undef AST.VoidType
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
  val <- cgen expr
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
      cgen body >>= ret
  
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


