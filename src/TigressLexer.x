{
{-# OPTIONS_GHC -w #-}
module TigressLexer where

import Prelude hiding (EQ, LT, GT)
import TigressToken

}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$ascii_x_esc = [\! \# \$ \% & \' \( \) \;:\<\=\>\?0-9 \+ \, \- \* \. \/ \@ A-Z\[\]\^ a-z \{ \_ \} ]
$esc = [ \\ \" trn0]

tokens :-

  $white+  ;
  "array" { \_->ARRAY }
  "break" { \_->BREAK }
  "do"    { \_->DO }
  "else"  { \_->ELSE }
  "end"   { \_->END }
  "for"   { \_->FOR }
  "function" { \_->FUNCTION }
  "if"    { \_->IF }
  "in"    { \_->IN }
  "let"   { \_->LET }
  "nil"   { \_->NIL }
  "of"    { \_->OF }
  "then"  { \_->THEN }
  "to"    { \_->TO }
  "type"  { \_->TYPE }
  "var"   { \_->VAR }
  "while" { \_->WHILE }
  "new"   { \_->NEW  }
  ","     { \_->COMMA }
  ":"     { \_->COLON }
  ";"     { \_->SEMICOLON}
  "("     { \_->LPAREN}
  ")"     { \_->RPAREN}
  "["     { \_->LBRACKET}
  "]"     { \_->RBRACKET}
  "{"     { \_->LBRACE}
  "}"     { \_->RBRACE}
  "."     { \_->DOT}
  "+"     { \_->PLUS}
  "-"     { \_->MINUS}
  "*"     { \_->MULT}
  "/"     { \_->DIV}
  "="     { \_->EQ}
  "<>"    { \_->NEQ}
  "<"     { \_->LT}
  "<="     { \_->LE}
  ">"     { \_->GT}
  ">="     { \_->GE}
  "&"     { \_ -> AND}
  "|"     { \_ -> OR}
  ":="    { \_ -> ASGN}
  $digit+ { \str -> INT $ read str}
  $alpha[$digit $alpha]* { ID }



