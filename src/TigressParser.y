{
{-# OPTIONS_GHC -w #-}
{- Parser for Tigress.-}
module TigressParser where

import TigressToken
import TigressExpr
import TigressLexer
import Prelude hiding (EQ, LT, GT)
import qualified Data.List as List
import Control.Exception (throw)
}

%name      tparse     expr
%tokentype {TigressToken}
%error     {parseError} 
%monad {Either String} {(>>=)} {Right}

%token
ARRAY {ARRAY}
BREAK {BREAK}
DO {DO}
ELSE {ELSE}
END {END}
FOR {FOR}
FUNCTION {FUNCTION}
IF {IF}
IN {IN}
LET {LET}
NIL {NIL}
OF {OF}
THEN {THEN}
TO {TO}
TYPE {TYPE}
VAR {VAR}
WHILE {WHILE}
NEW {NEW}
"," {COMMA}
":" {COLON}
";" {SEMICOLON}
"(" {LPAREN}
")" {RPAREN}
"[" {LBRACKET}
"]" {RBRACKET}
"{" {LBRACE}
"}" {RBRACE}
"." {DOT}
"+" {PLUS}
"-" {MINUS}
"*" {MULT}
"/" {DIV}
"=" {EQ}
"<>" {NEQ}
"<" {LT}
"<=" {LE}
">" {GT}
">=" {GE}
"&" {AND}
"|" {OR}
":=" {ASGN}
ID {ID $$}
INT {INT $$}

%nonassoc prec_ifdangling_noelse
%nonassoc ELSE
%nonassoc prec_single_if
%nonassoc ":="
%left "|"
%left "&"
%nonassoc "=" "<>" "<" ">" "<=" ">="
%left "+" "-"
%left "*" "/"
%right UNARY_MINUS

%%
{-

operator precedence:
- (unary)
* /
+ -
= <> > < >= <=
&
|
:=

-}

expr:
  if_expr_not_dangling { $1 }
| if_expr_dangling     { $1 }
;

if_expr_not_dangling:
  oexpr1 %prec prec_single_if { $1 }
| if_then_not_dangling ELSE if_expr_not_dangling { case $1 of (x,y) -> EIfElse x y $3 }
| WHILE expr DO if_expr_not_dangling { EWhile $2 $4 }
| FOR id ":=" expr TO expr DO if_expr_not_dangling { EFor $2 $4 $6 $8 }
;

if_expr_dangling:
  if_then_dangling { $1 }
| if_then_not_dangling
   %prec prec_ifdangling_noelse { case $1 of (x,y) -> EIf x y }
| if_then_not_dangling ELSE if_expr_dangling { case $1 of (x,y) -> EIfElse x y $3 }
| WHILE expr DO if_expr_dangling { EWhile $2 $4 }
| FOR id ":=" expr TO expr DO if_expr_dangling { EFor $2 $4 $6 $8 }
;

if_then_not_dangling:
  IF expr THEN if_expr_not_dangling { ($2, $4) }
;

if_then_dangling:
  IF expr THEN if_expr_dangling { EIf $2 $4 }
;


oexpr1: -- "|"
  lvalue ":=" oexpr1 {EAsgn $1 $3 }
| oexpr1 "|" oexpr1 { EBin BOr $1 $3 }
| oexpr1 "&" oexpr1 { EBin BAnd $1 $3 }
| oexpr1 "=" oexpr1 { EBin BEq $1 $3 }
| oexpr1 "<>" oexpr1 { EBin BNeq $1 $3 }
| oexpr1 ">" oexpr1 { EBin BGt $1 $3 }
| oexpr1 "<" oexpr1 { EBin BLt $1 $3 }
| oexpr1 ">=" oexpr1 { EBin BGe $1 $3 }
| oexpr1 "<=" oexpr1 { EBin BLe $1 $3 }
| oexpr1 "+" oexpr1 { EBin BAdd $1 $3 }
| oexpr1 "-" oexpr1 { EBin BSub $1 $3 }
| oexpr1 "*" oexpr1 { EBin BMul $1 $3 }
| oexpr1 "/" oexpr1 { EBin BDiv $1 $3 }
| simpl_expr  { $1 }
| "-" simpl_expr %prec UNARY_MINUS { EMinus $2 }
;

simpl_expr:
  INT  { EInt $1 }
| NIL  { ENil }
| lvalue { ELValue $1 }
| id "(" ")" { EApp $1 [] }
| id "(" expr_list ")" { EApp $1 $3 }
| "(" ")" { ESeq [] }
| "(" expr_seq ")" { ESeq $2 }
| type_id "{" field_list "}" { ERec $1 $3 }
| NEW type_id "[" expr "]" OF expr { EArr $2 $4 $7 }
| BREAK  { EBreak }
| LET declaration_list IN END          { ELet $2 [] }
| LET declaration_list IN expr_seq END { ELet $2 $4 }
;


expr_seq:
  expr  { [$1] }
| expr ";" expr_seq { $1 : $3 }
;

expr_list:
  expr  { [$1] }
| expr "," expr_list { $1 : $3 }
;

field_list:
  id "=" expr { [Field $1 $3] }
| id "=" expr "," field_list { Field $1 $3 : $5 }
;

lvalue:
  id  { LId $1 }
| lvalue "." id { LMem $1 $3}
| lvalue "[" expr "]" { LIdx $1 $3 }
;

declaration_list:
  declaration { [$1] }
| declaration declaration_list { $1 : $2 }
;

declaration:
  type_declaration { DType $1 }
| variable_declaration { DVar $1 }
| function_declaration { DFun $1 }
;

type_declaration:
  TYPE type_id "=" type { TypeDec $2 $4 }
;

type:
  type_id { TId $1 }
| "{" "}" { TFields [] }
| "{" type_fields "}" { TFields $2 }
| ARRAY OF type_id { TAry $3 }
;

type_fields:
  type_field { [$1] }
| type_fields "," type_field { $1 ++ [$3] }
;

type_field:
  id ":" type_id { TypeField $1 $3 }
;

variable_declaration:
  VAR id ":=" expr             { VarDec $2 Nothing   $4 }
| VAR id ":" type_id ":=" expr { VarDec $2 (Just $4) $6 }
;

function_declaration:
  FUNCTION id "(" type_fields ")" "=" expr             { FunDec $2 $4 Nothing   $7 }
| FUNCTION id "(" type_fields ")" ":" type_id "=" expr { FunDec $2 $4 (Just $7) $9 }
;
id: ID { Id $1 };

type_id: ID { TypeId $1 };

{

parseError :: [TigressToken] -> Either String a
parseError toks = Left $ "parseError: " ++ show toks

}
