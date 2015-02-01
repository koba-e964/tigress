module Typing where

import TigressExpr
import Control.Monad (when)
import Control.Monad.State (State, evalState)

data AType
  = CType !Type -- concrete type
  | TVar !String -- type variable
   deriving (Eq, Show)


data TypedExpr = 
  TEStr !String -- not used in Tigress. This constructor is prepared for future extension.
  | TEInt !Integer
  | TENil AType
  | TELValue !LValue !AType
  | TEMinus !TypedExpr
  | TEBin !BinOp !TypedExpr !TypedExpr
  | TEAsgn !LValue !TypedExpr
  | TEApp !Id ![TypedExpr]
  | TESeq ![TypedExpr]
  | TERec !TypeId ![Field] -- new record
  | TEArr !TypeId !TypedExpr !TypedExpr        -- new array
  | TEIf !TypedExpr !TypedExpr
  | TEIfElse !TypedExpr !TypedExpr !TypedExpr
  | TEWhile !TypedExpr !TypedExpr
  | TEFor !Id !TypedExpr !TypedExpr !TypedExpr
  | TEBreak
  | TELet ![Dec] ![TypedExpr]
  deriving (Eq, Show)

type Substitution = [(String, AType)]
type M = State Int

runM :: M a -> a
runM m = evalState m 0


stringType :: Type
intType :: Type

stringType = TId (TypeId "string")
intType = TId (TypeId "int")

getTypedExpr :: Expr -> TypedExpr
typeOf :: TypedExpr -> Type
aTypeOf :: TypedExpr -> AType
isValidExpr :: TypedExpr -> Bool

addType ::  Expr -> M TypedExpr
unifyExpr :: TypedExpr -> M Substitution
substExpr :: Substitution -> TypedExpr -> TypedExpr

getTypedExpr expr = runM $ do
  texpr <- addType expr
  subst <- unifyExpr texpr
  let ret = substExpr subst texpr
  when (isValidExpr ret) $ fail $ "Type inference failed: " ++ show ret
  return ret

typeOf = undefined
aTypeOf = undefined
isValidExpr = undefined



addType = undefined
unifyExpr = undefined
substExpr = undefined
