module TigressExpr where

import Data.Map (Map)

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
  deriving (Eq, Show)

data Field =
  Field !Id !Expr deriving (Eq, Show)

data LValue =
  LId !Id
  | LMem !LValue !Id
  | LIdx !LValue !Expr
   deriving (Eq, Show)
newtype Id = Id String deriving (Eq, Show)

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
   deriving (Eq, Show, Ord)

-- declarations
data Dec =
  DType !TypeDec
  | DVar !VarDec
  | DFun !FunDec
   deriving (Eq, Show)
data TypeDec = TypeDec !TypeId !Type deriving (Eq, Show)
data Type = 
  TId !TypeId
  | TFields ![TypeField]
  | TAry !TypeId
   deriving (Eq, Show)
data TypeField = TypeField !Id !TypeId deriving (Eq, Show)

newtype TypeId = TypeId String deriving (Eq, Show)

data VarDec = 
  VarDec !Id !(Maybe TypeId) !Expr deriving (Eq, Show) -- type-id can be Nothing. type Nothing is the type of VNone.

data FunDec = FunDec !Id ![TypeField] !(Maybe TypeId) !Expr deriving (Eq, Show)

