module Main where

import Control.Monad.ST (runST)
import Control.Monad.Identity (runIdentity)
import System.Timeout (timeout)
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

evalPure :: Expr -> Either String FreezedValue
evalPure expr = runST (TE.runTigressExpr expr)

-- | Tests two values are equal. Checking will be terminated after 2000 milliseconds.
testEq :: (Eq a, Show a) => String -> a -> a -> TF.Test
testEq msg actual expected = TFH.testCase msg $ do
  result <- timeout 2000000 $ TH.assertEqual msg expected actual
  case result of
    Nothing -> fail "time limit exceeded (2000ms)"
    _       -> return ()

evalCheck :: String -> String -> FreezedValue -> TF.Test
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
  ,testsLoop
  ,testsArray
 ]

testsAdd :: TF.Test
testsAdd = TF.testGroup "+" [
  evalCheck "eval_add1" "1+1" (FVInt 2)
  ,evalCheck "eval_add2" "-19+4" (FVInt (-15))
  ,evalCheck "eval_add3" "-1234567890+12345678901" (FVInt 11111111011)
 ]
testsDiv :: TF.Test
testsDiv = TF.testGroup "/" [
  checkErrorExpr "eval_div1" "3/0"
  ,evalCheck "eval_div2" "3/4" (FVInt 0)
  ,evalCheck "eval_div3" "-5/4" (FVInt (-2)) -- round to -inf
 ]
testsCmp :: TF.Test
testsCmp = TF.testGroup "eval_cmp" [
  evalCheck "eval_cmp1" "2<3" (FVInt 1)
  ,evalCheck "eval_cmp2" "2147483648 < 1" (FVInt 0)
  ,evalCheck "eval_cmp3" "4294967297 = 1" (FVInt 0)
 ]
testsLogic :: TF.Test
testsLogic = TF.testGroup "eval_logic" [
  evalCheck "eval_and1" "2 & 3"   (FVInt 1)
  ,evalCheck "eval_and2" "0 & 1/0"   (FVInt 0) -- & is lazy and does not evaluate 1/0
  ,evalCheck "eval_and3" "2 & 0"   (FVInt 0)
  ,evalCheck "eval_or1" "2 | 0"   (FVInt 1)
  ,evalCheck "eval_or2" "1 | 1/0"   (FVInt 1) -- | is lazy and does not evaluate 1/0
  ,evalCheck "eval_or3" "0 | 100"   (FVInt 1)
  ,evalCheck "eval_or4" "0 | 0"   (FVInt 0)
 ]
testsLet :: TF.Test
testsLet = TF.testGroup "eval_let" [
  evalCheck "eval_let1" "let var a := 1 in a + 2 end" (FVInt 3)
  ,evalCheck "eval_let2" "let var a := 1 function b (i:int) : int = i + i in b(a) + 2 end" (FVInt 4)
  ,evalCheck "eval_let3" "let function fib(i:int):int = if i <= 1 then i else fib(i-1) + fib(i-2) in fib(10) end" (FVInt 55) -- recursion
  ,evalCheck "eval_let4" "let function odd(i:int) : int = if i = 0 then 0 else even(i-1) function even (i:int) : int = if i = 0 then 1 else odd(i-1) in odd(99) end" (FVInt 1) -- mutual recursion
 ]

testsLoop :: TF.Test
testsLoop = TF.testGroup "eval_loop" [
  evalCheck "eval_for1" "for i := 0 to 10 do 2" FVNone -- "for" returns no value
  ,evalCheck "eval_for2" "let var sum := 0 in for i := 0 to 10 do sum := sum + i; sum end" (FVInt 55)
  ,evalCheck "eval_while1" "let var sum := 0 var i := 0 in while i <= 10 do (sum := sum + i; i := i + 1); sum end" (FVInt 55)
  ,evalCheck "eval_while2" "let var i := 1 in while i <= 100 do i := i + i; i end" (FVInt 128)  
  ,evalCheck "eval_break1" "let var sum := 0 in for i := 0 to 10 do (sum := sum + i; if i >= 4 then break); sum end" (FVInt 10)
  ,evalCheck "eval_break2" "let var i := 1 in while 1 do (i := i + i; if i >= 100 then break); i end" (FVInt 128)
  ,checkErrorExpr "eval_break3" "(for i := 0 to 10 do (i;());break)"
 ]

testsArray :: TF.Test
testsArray = TF.testGroup "eval_array" [
  evalCheck "eval_array1" "let type intArray = array of int var ary := new intArray [8] of 0 in ary[3] end" (FVInt 0)
  ,evalCheck "eval_array2" "let type ints = array of int var ary := new ints[100] of 0 in for i := 0 to 99 do (ary[i] := i); ary[53] end" (FVInt 53)
  ,checkErrorExpr "eval_array3" "let type ints = array of int var ary := new ints[100] of 0 in ary[100] end"
 ]

