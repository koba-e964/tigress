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

tests :: [TF.Test]
tests = [testParsing]

testParsing :: TF.Test
testParsing = TF.testGroup "parsing" [
  testEq "-8" (exprOfString "-8") (EMinus (EInt 8)),
  testEq "(1+2) * (3 * 4 + 5 * 6)" (exprOfString "(1+2) * (3 * 4 + 5 * 6)") (EBin BMul (ESeq [EBin BAdd (EInt 1) (EInt 2)]) (ESeq [EBin BAdd (EBin BMul (EInt 3) (EInt 4)) (EBin BMul (EInt 5) (EInt 6))]))
 ]
