{- The module for tokens in Tigress. String is not supported. -}

module TigressToken where

data TigressToken = 
  ARRAY | BREAK | DO | ELSE | END | FOR
  | FUNCTION | IF | IN | LET | NIL | OF | THEN | TO | TYPE | VAR | WHILE | NEW {- original keyword for Tigress -}
  | COMMA | COLON | SEMICOLON | LPAREN | RPAREN | LBRACKET | RBRACKET
  | LBRACE | RBRACE | DOT | PLUS | MINUS | MULT | DIV
  | EQ | NEQ | LT | LE | GT | GE | AND | OR | ASGN
  | ID String | INT Integer
   deriving (Eq, Show)

