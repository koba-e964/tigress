{-# LANGUAGE BangPatterns #-}
module TigressExpr where

data Expr = 
  EStr !String -- not used in Tigress. This constructor is prepared for future extension.
  | EInt !Integer
  | ENil
  | ELValue !LValue
  | EMinus !Expr
  | EBin !BinOp !Expr !Expr
  | EAsgn !LValue !Expr
  | EApp !Id ![Expr]
  | ESeq ![Expr]
  | ERec !TypeId ![Field] -- new record
  | EArr !TypeId !Expr !Expr        -- new array
  | EIf !Expr !Expr
  | EIfElse !Expr !Expr !Expr
  | EWhile !Expr !Expr
  | EFor !Id !Expr !Expr !Expr
  | EBreak
  | ELet ![Dec] ![Expr]
  deriving (Show)

data Field =
  Field !Id !Expr deriving Show

data LValue =
  LId !Id
  | LMem !LValue !Id
  | LIdx !LValue !Expr
   deriving Show
newtype Id = Id String deriving Show

data BinOp =
 BAdd
 | BSub
 | BMul
 | BDiv
 | BEq
 | BNeq
 | BLt
 | BLe
 | BGt
 | BGe
 | BAnd
 | BOr
   deriving Show

-- declarations
data Dec =
  DType !TypeDec
  | DVar !VarDec
  | DFun !FunDec
   deriving Show
data TypeDec = TypeDec !TypeId !Type deriving Show
data Type = 
  TId !TypeId
  | TFields ![TypeField]
  | TAry !TypeId
   deriving Show
data TypeField = TypeField !Id !TypeId deriving Show

newtype TypeId = TypeId String deriving Show

data VarDec = 
  VarDec !Id !(Maybe TypeId) !Expr deriving Show -- type-id can be Nothing.

data FunDec = FunDec !Id ![TypeField] !(Maybe TypeId) !Expr deriving Show



