{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TigressEval where

import TigressExpr
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)

type Pos = Int -- the position of variable
type VarMap m = Map String (VarRef m Value)
type VarRef m = MutVar (PrimState m)


data TigState m = TigState {
    varmap :: VarMap m,
    count  :: Int
}

newtype TigressT m a = TigressT { invTigress :: ExceptT String (StateT (TigState m) m) a } deriving (Functor, Applicative, Monad, MonadState (TigState m), MonadError String)

instance MonadTrans TigressT where
  lift = TigressT . lift . lift

emptyTigState :: TigState m
emptyTigState = TigState Map.empty 0

runTigress :: (PrimMonad m, Monad m) => TigressT m a -> m (Either String a)
runTigress m = evalStateT (runExceptT (invTigress m)) emptyTigState

newVariable :: (PrimMonad m, Monad m) => TigressT m (VarRef m Value)
newVariable = lift $ newMutVar VNone


getVar :: (PrimMonad m, Monad m) => LValue -> TigressT m (VarRef m Value)
getVar (LId (Id name)) = do
   m <- liftM varmap get
   case Map.lookup name m of
     Just p  -> return p
     Nothing -> throwError $ "undefined variable: " ++ name
getVar (LMem _ _) = throwError "TODO: getVar-mem"
getVar (LIdx lv e) = throwError "TODO: getVar-index"

readVar :: (PrimMonad m, Monad m) => VarRef m Value -> TigressT m Value
readVar var = lift $ readMutVar var
updateVar :: (PrimMonad m, Monad m) => VarRef m Value -> Value -> TigressT m ()
updateVar var value = lift $ writeMutVar var value

ensureInt :: (PrimMonad m, Monad m) => Value -> TigressT m Integer
ensureInt (VInt v) = return v
ensureInt value    = throwError $ "not an integer: " ++ show value

eval :: (PrimMonad m, Monad m) => Expr -> TigressT m Value
eval (EStr str) = return $ VStr str
eval (EInt i)   = return $ VInt i
eval ENil       = return VNil
eval (ELValue lv) = do
    var <- getVar lv
    readVar var
eval (EMinus e) = do
  v <- eval e
  iv <- ensureInt v
  return $ VInt (- iv)
eval (EBin op e1 e2) = do
  case op of
    BAnd -> iand e1 e2
    BOr  -> ior e1 e2
    BDiv -> idiv e1 e2
    _    -> do
      i1 <- ensureInt =<< eval e1
      i2 <- ensureInt =<< eval e2
      let ops = [(BAdd,(+)), (BSub,(-)), (BMul,(*)), (BEq,bToI (==)), (BNeq,bToI (/=)), (BGt,bToI (>)), (BLt,bToI (<)),
           (BGe,bToI (>=)), (BLe,bToI (<=))] :: [(BinOp, Integer->Integer->Integer)]
      let opInt = fromMaybe undefined (lookup op ops)
      return $ VInt $ opInt i1 i2
   where
         bToI op x y = if op x y then 1 else 0
         iand x y = do
           ix <- ensureInt =<< eval x
           case ix of 
             0 -> return $ VInt 0
             _ -> do
               iy <- ensureInt =<< eval y
               case iy of {0 -> return $ VInt 0; _ -> return $ VInt 1;}
         ior x y = do
           ix <- ensureInt =<< eval x
           case ix of 
             0 -> do
               iy <- ensureInt =<< eval y
               case iy of {0 -> return $ VInt 0; _ -> return $ VInt 1;}
             _ -> return $ VInt 1
         idiv x y = do
           ix <- ensureInt =<< eval x
           iy <- ensureInt =<< eval y
           when (iy == 0) (throwError "Divide by zero")
           return $ VInt $ ix `div` iy

eval (EAsgn lv e) = do
  var <- getVar lv
  value <- eval e
  updateVar var value
  return VNone
eval (EApp _ _) = throwError "TODO: application"
eval (ESeq ls) = do
  results <- forM ls eval
  return $ last (VNone : results)
eval (ERec _ _) = throwError "TODO: record"
eval (EArr _ _ _) = throwError "TODO: new-array"
eval (EIf cond e1) = do
  b <- ensureInt =<< eval cond
  when (b /= 0) (eval e1 >> return ()) -- a nonzero value is regarded to be true.
  return VNone -- always returns no value.
eval (EIfElse cond e1 e2) = do
  b <- ensureInt =<< eval cond
  case b of
    0 -> eval e2 -- 0 is regarded to be false.
    _ -> eval e1 -- otherwise, an integer is regarded to be true.
eval (EWhile _ _) = throwError "TODO: while"
eval (EFor _ _ _ _) = throwError "TODO: for"
eval EBreak       = throwError "TODO: break"
eval (ELet decs expr)   = throwError "TODO: let"

addDec :: (PrimMonad m, Monad m) => Dec -> TigressT m (VarMap m)
addDec (DType _) = liftM varmap get
addDec (DVar (VarDec (Id id) ty expr)) = do
  var <- newVariable
  val <- eval expr
  updateVar var val
  vm <- liftM varmap $ get
  let newvm = Map.insert id var vm
  modify $ \s -> s { varmap = newvm }
  return newvm
addDec (DFun (FunDec id params ty expr)) = throwError "TODO: function declaration"



