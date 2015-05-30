--------------------------------------------------------------------
-- |
-- Module    :  JIT
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module JIT where

import Control.Monad.Except (ExceptT)
import Data.Int (Int64)
import Foreign.Ptr ( FunPtr, castFunPtr )

import qualified LLVM.General.AST as AST
import LLVM.General.Context (Context)
import qualified LLVM.General.ExecutionEngine as EE
import qualified LLVM.General.Module as Mod

import Codegen (withContextT)



foreign import ccall "dynamic" haskFun :: FunPtr (IO Int64) -> IO Int64

run :: FunPtr a -> IO Int64
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int64)) --- functions in tigress should return int64

jit :: Context ->  (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

-- | Takes a module, executes it, and returns the result.
runJIT :: AST.Module -> ExceptT String IO (Maybe Int64)
runJIT astmod =
  withContextT $ \context ->
    Mod.withModuleFromAST context astmod $ \m ->
      jit context $ \executionEngine ->
        -- Execution. Slightly optimized by jit compiler.
        EE.withModuleInEngine executionEngine m $ \ee -> do
          mainfn <- EE.getFunction ee (AST.Name "main")
          case mainfn of
            Just fn -> do
              res <- run fn
              return $ Just res
            Nothing -> return Nothing

