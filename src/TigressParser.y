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

%%

expr:
  INT  { EInt $1 }
| NIL  { ENil }
| lvalue { ELValue $1 }
| "-" expr { EMinus $2 }
| expr binary_operator expr { EBin $2 $1 $3 }
| lvalue ":=" expr {EAsgn $1 $3 }
| id "(" ")" { EApp $1 [] }
| id "(" expr_list ")" { EApp $1 $3 }
| "(" expr_seq ")" { ESeq $2 }
| type_id "{" field_list "}" { ERec $1 $3 }
| type_id "[" expr "]" OF expr { EArr $1 $3 $6 }
| IF expr THEN expr { EIf $2 $4 }
| IF expr THEN expr ELSE expr { EIfElse $2 $4 $6 }
| WHILE expr DO expr { EWhile $2 $4 }
| FOR id ":=" expr TO expr DO expr { EFor $2 $4 $6 $8 }
| BREAK  { EBreak }
| LET declaration_list IN END          { ELet $2 [] }
| LET declaration_list IN expr_seq END { ELet $2 $4 }
;

expr_seq:
  expr  { [$1] }
| expr_seq ";" expr { $1 ++ [$3] }
;

expr_list:
  expr  { [$1] }
| expr_list "," expr { $1 ++ [$3] }
;

field_list:
  id "=" expr { [Field $1 $3] }
| field_list "," id "=" expr { $1 ++ [Field $3 $5] }
;

lvalue:
  id  { LId $1 }
| lvalue "." id { LMem $1 $3}
| lvalue "[" expr "]" { LIdx $1 $3 }
;

declaration_list:
  declaration { [$1] }
| declaration_list declaration { $1 ++ [$2] }
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

binary_operator:
  "+"  { BAdd }
| "-"  { BSub }
| "*"  { BMul }
| "/"  { BDiv }
| "="  { BEq  }
| "<>" { BNeq }
| "<"  { BLt  }
| "<=" { BLe  }
| ">"  { BGt  }
| ">=" { BGe  }
| "&"  { BAnd }
| "|"  { BOr  }
;

{

parseError :: [TigressToken] -> Either String a
parseError toks = Left $ "parseError: " ++ show toks

}
