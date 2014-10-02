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

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Except

import LLVM.General.Target
import LLVM.General.Context
import LLVM.General.CodeModel
import LLVM.General.Module as Mod
import qualified LLVM.General.AST as AST

import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.Analysis

import qualified LLVM.General.ExecutionEngine as EE

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

-- | Returns passes whose level is 'opt'.
passes :: Word -> PassSetSpec
passes opt = defaultCuratedPassSetSpec { optLevel = Just opt }

-- | Takes a module, optimizes it, executes it, and returns the optimized module.
-- | The level of optimization is specified by 'opt'.
runJIT :: AST.Module -> Word -> IO (Either String AST.Module)
runJIT mod opt = do
  withContext $ \context -> do
    runExceptT $ withModuleFromAST context mod $ \m -> do
      optmod <- withPassManager (passes opt) $ \pm -> do
        -- Optimization Pass
        runPassManager pm m
        moduleAST m
      jit context $ \executionEngine ->
          -- Execution. Slightly optimized by jit compiler.
          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.Name "main")
            case mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()
      -- Return the optimized module
      return optmod

