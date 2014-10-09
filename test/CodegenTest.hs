module Main where

import Control.Monad.Except
import Data.Either (isLeft)
import Data.Int
import qualified LLVM.General.AST as AST
import LLVM.General.Module as Mod
import qualified LLVM.General.ExecutionEngine as EE
import System.Timeout (timeout)
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH 
import qualified Test.HUnit as TH
import TigressExpr
import TigressLexer as TL
import TigressParser as TP
import Codegen
import Emit
import JIT


main :: IO ()
main = TF.defaultMain tests

exprOfString :: String -> Expr
exprOfString str = 
  let toks = TL.alexScanTokens str
      exprOrErr = TP.tparse toks in
  case exprOrErr of
    Right x -> x
    Left  _ -> error $ "no parse: " ++ str


-- | Generates a module, run function "main" and returns its value (if there's any).
genModule :: Expr -> ExceptT String IO (AST.Module, Maybe Int64)
genModule expr = do
  newmod <- liftEither $ codegen (emptyModule "genmod_test") [expr]
  withContextT $ \context -> do
    withModuleFromAST context newmod $ \m -> do
      jit context $ \executionEngine ->
        -- Execution. Slightly optimized by jit compiler.
        EE.withModuleInEngine executionEngine m $ \ee -> do
          mainfn <- EE.getFunction ee (AST.Name "main")
          case mainfn of
            Just fn -> do
              res <- run fn
              return (newmod, Just res)
            Nothing -> return (newmod, Nothing)


-- | Tests two values are equal. Checking will be terminated after 2000 milliseconds.
assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq msg actual expected = do
  result <- timeout 2000000 $ TH.assertEqual msg expected actual
  case result of
    Nothing -> fail "time limit exceeded (2000ms)"
    _       -> return ()

evalCheck :: String -> String -> Maybe Int64 -> TF.Test
evalCheck name str expected = TFH.testCase name $ do
  (_newmod, result) <- liftError $ genModule (exprOfString str)
  assertEq name result expected
checkErrorExpr :: String -> String -> TF.Test
checkErrorExpr name str = TFH.testCase name $ TH.assertBool "failure expected" =<<
  let expr = exprOfString str in do
    result <- runExceptT (genModule expr)
    return $ isLeft result
tests :: [TF.Test]
tests = [
  testsAdd
  ,testsDiv
  ,testsCmp
  ,testsLogic
  ,testsLet
  ,testsLoop
  ,testsArray
 ]

testsAdd :: TF.Test
testsAdd = TF.testGroup "+" [
  evalCheck "eval_add1" "1+1" (Just 2)
  ,evalCheck "eval_add2" "-19+4" (Just (-15))
  ,evalCheck "eval_add3" "-1234567890+12345678901" (Just 11111111011)
 ]
testsDiv :: TF.Test
testsDiv = TF.testGroup "/" [
--  checkErrorExpr "eval_div1" "3/0"
  evalCheck "eval_div2" "3/4" (Just 0)
  ,evalCheck "eval_div3" "-5/4" (Just (-2)) -- round to -inf
 ]
testsCmp :: TF.Test
testsCmp = TF.testGroup "eval_cmp" [
  evalCheck "eval_cmp1" "2<3" (Just 1)
  ,evalCheck "eval_cmp2" "2147483648 < 1" (Just 0)
  ,evalCheck "eval_cmp3" "4294967297 = 1" (Just 0)
 ]
testsLogic :: TF.Test
testsLogic = TF.testGroup "eval_logic" [
  evalCheck "eval_and1" "2 & 3"   (Just 1)
  ,evalCheck "eval_and2" "0 & 1/0"   (Just 0) -- & is lazy and does not evaluate 1/0
  ,evalCheck "eval_and3" "2 & 0"   (Just 0)
  ,evalCheck "eval_or1" "2 | 0"   (Just 1)
  ,evalCheck "eval_or2" "1 | 1/0"   (Just 1) -- | is lazy and does not evaluate 1/0
  ,evalCheck "eval_or3" "0 | 100"   (Just 1)
  ,evalCheck "eval_or4" "0 | 0"   (Just 0)
  ,evalCheck "eval_not1" "not(0)"   (Just 1)
  ,evalCheck "eval_not2" "not(100)"   (Just 0)
 ]
testsLet :: TF.Test
testsLet = TF.testGroup "eval_let" [
  evalCheck "eval_let1" "let var a := 1 in a + 2 end" (Just 3)
  ,evalCheck "eval_let2" "let var a := 1 function b (i:int) : int = i + i in b(a) + 2 end" (Just 4)
  ,evalCheck "eval_let3" "let function fib(i:int):int = if i <= 1 then i else fib(i-1) + fib(i-2) in fib(10) end" (Just 55) -- recursion
  ,evalCheck "eval_let4" "let function odd(i:int) : int = if i = 0 then 0 else even(i-1) function even (i:int) : int = if i = 0 then 1 else odd(i-1) in odd(99) end" (Just 1) -- mutual recursion
 ]

testsLoop :: TF.Test
testsLoop = TF.testGroup "eval_loop" [
  evalCheck "eval_for1" "for i := 0 to 10 do 2" Nothing -- "for" returns no value
  ,evalCheck "eval_for2" "let var sum := 0 in for i := 0 to 10 do sum := sum + i; sum end" (Just 55)
  ,evalCheck "eval_while1" "let var sum := 0 var i := 0 in while i <= 10 do (sum := sum + i; i := i + 1); sum end" (Just 55)
  ,evalCheck "eval_while2" "let var i := 1 in while i <= 100 do i := i + i; i end" (Just 128)  
  ,evalCheck "eval_break1" "let var sum := 0 in for i := 0 to 10 do (sum := sum + i; if i >= 4 then break); sum end" (Just 10)
  ,evalCheck "eval_break2" "let var i := 1 in while 1 do (i := i + i; if i >= 100 then break); i end" (Just 128)
  ,checkErrorExpr "eval_break3" "(for i := 0 to 10 do (i;());break)"
 ]

testsArray :: TF.Test
testsArray = TF.testGroup "eval_array" [
  evalCheck "eval_array1" "let type intArray = array of int var ary := new intArray [8] of 0 in ary[3] end" (Just 0)
  ,evalCheck "eval_array2" "let type ints = array of int var ary := new ints[100] of 0 in for i := 0 to 99 do (ary[i] := i); ary[53] end" (Just 53)
  ,checkErrorExpr "eval_array3" "let type ints = array of int var ary := new ints[100] of 0 in ary[100] end"
 ]

