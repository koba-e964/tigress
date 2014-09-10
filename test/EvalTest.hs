module Main where

import Control.Monad.ST (runST)
import Control.Monad.Identity (runIdentity)
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH 
import qualified Test.HUnit as TH
import TigressExpr
import TigressLexer as TL
import TigressParser as TP
import TigressEval as TE


main :: IO ()
main = TF.defaultMain tests

exprOfString :: String -> Expr
exprOfString str = 
  let toks = TL.alexScanTokens str
      exprOrErr = TP.tparse toks in
  case exprOrErr of
    Right x -> x
    Left  _ -> error $ "no parse: " ++ str

evalPure :: Expr -> Either String Value
evalPure expr = runST (TE.runTigress (TE.eval expr))

testEq :: (Eq a, Show a) => String -> a -> a -> TF.Test
testEq msg actual expected = TFH.testCase msg (TH.assertEqual msg expected actual)

evalCheck :: String -> String -> Value -> TF.Test
evalCheck name str expected = 
  testEq name (evalPure (exprOfString str)) (Right expected)
checkErrorExpr :: String -> String -> TF.Test
checkErrorExpr name str = TFH.testCase name $ TH.assertBool "failure expected" $
  let expr = exprOfString str in
  case evalPure expr of
    Right _ -> False
    Left  _ -> True

tests :: [TF.Test]
tests = [
  testsAdd
  ,testsDiv
  ,testsCmp
  ,testsLogic
  ,testsLet
 ]

testsAdd :: TF.Test
testsAdd = TF.testGroup "+" [
  evalCheck "eval_add1" "1+1" (VInt 2)
  ,evalCheck "eval_add2" "-19+4" (VInt (-15))
  ,evalCheck "eval_add3" "-1234567890+12345678901" (VInt 11111111011)
 ]
testsDiv :: TF.Test
testsDiv = TF.testGroup "/" [
  checkErrorExpr "eval_div1" "3/0"
  ,evalCheck "eval_div2" "3/4" (VInt 0)
  ,evalCheck "eval_div3" "-5/4" (VInt (-2)) -- round to -inf
 ]
testsCmp :: TF.Test
testsCmp = TF.testGroup "eval_cmp" [
  evalCheck "eval_cmp1" "2<3" (VInt 1)
  ,evalCheck "eval_cmp2" "2147483648 < 1" (VInt 0)
  ,evalCheck "eval_cmp3" "4294967297 = 1" (VInt 0)
 ]
testsLogic :: TF.Test
testsLogic = TF.testGroup "eval_logic" [
  evalCheck "eval_and1" "2 & 3"   (VInt 1)
  ,evalCheck "eval_and2" "0 & 1/0"   (VInt 0) -- & is lazy and does not evaluate 1/0
  ,evalCheck "eval_and3" "2 & 0"   (VInt 0)
  ,evalCheck "eval_or1" "2 | 0"   (VInt 1)
  ,evalCheck "eval_or2" "1 | 1/0"   (VInt 1) -- | is lazy and does not evaluate 1/0
  ,evalCheck "eval_or3" "0 | 100"   (VInt 1)
  ,evalCheck "eval_or4" "0 | 0"   (VInt 0)
 ]
testsLet :: TF.Test
testsLet = TF.testGroup "eval_let" [
  evalCheck "eval_let1" "let var a := 1 in a + 2 end" (VInt 3)
  ,evalCheck "eval_let2" "let var a := 1 function b (i:int) : int = i + i in b(a) + 2 end" (VInt 4)
  ,evalCheck "eval_let3" "let function fib(i:int):int = if i <= 1 then i else fib(i-1) + fib(i-2) in fib(10) end" (VInt 55) -- recursion
  ,evalCheck "eval_let4" "let function odd(i:int) : int = if i = 0 then 0 else even(i-1) function even (i:int) : int = if i = 0 then 1 else odd(i-1) in odd(99) end" (VInt 1) -- mutual recursion
  ,evalCheck "eval_for1" "for i := 0 to 10 do 2" VNone -- "for" returns no value
  ,evalCheck "eval_for2" "let var sum := 0 in (for i := 0 to 10 do sum := sum + i); sum end" (VInt 55)  
 ]
