module Main where

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

testEq :: (Eq a, Show a) => String -> a -> a -> TF.Test
testEq msg actual expected = TFH.testCase msg (TH.assertEqual msg expected actual)

checkInvalidExpr :: String -> TF.Test
checkInvalidExpr str = TFH.testCase ("check_fail " ++ show str) $ TH.assertBool "failure expected" $
  let toks = TL.alexScanTokens str
      exprOrErr = TP.tparse toks in
  case exprOrErr of
    Right _ -> False
    Left  _ -> True

tests :: [TF.Test]
tests = [testParsing]

testParsing :: TF.Test
testParsing = TF.testGroup "parsing" [
  testEq "-8" (exprOfString "-8") (EMinus (EInt 8)),
  testEq "(1+2) * (3 * 4 + 5 * 6)" (exprOfString "(1+2) * (3 * 4 + 5 * 6)") (EBin BMul (ESeq [EBin BAdd (EInt 1) (EInt 2)]) (ESeq [EBin BAdd (EBin BMul (EInt 3) (EInt 4)) (EBin BMul (EInt 5) (EInt 6))])),
  testEq "op_precedence1" (exprOfString "2*5+3*4") (EBin BAdd (EBin BMul (EInt 2) (EInt 5)) (EBin BMul (EInt 3) (EInt 4))),
  testEq "op_precedence2" (exprOfString "a := b * 4") (EAsgn (LId (Id "a")) (EBin BMul (ELValue (LId (Id "b"))) (EInt 4))),
  checkInvalidExpr "a := b := 1",
  checkInvalidExpr "2 = 4 <= 5",
  testEq "op_paren1" (exprOfString "2 = (3 = 4)") (EBin BEq (EInt 2) (ESeq [EBin BEq (EInt 3) (EInt 4)])),
  testEq "op_assoc1" (exprOfString "1 + 2 + 3") (EBin BAdd (EBin BAdd (EInt 1) (EInt 2)) (EInt 3)),
  testEq "op_gen1"   (exprOfString "a.b := c")  (EAsgn (LMem (LId (Id "a")) (Id "b")) (ELValue (LId (Id "c"))))
 ]

