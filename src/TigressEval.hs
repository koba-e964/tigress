{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TigressEval where

import TigressExpr
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State (MonadState, StateT, evalStateT, get, modify, put)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.MutVar (MutVar, modifyMutVar, newMutVar, readMutVar, writeMutVar)

type Pos = Int -- the position of variable


data TigState m = TigState {
    varmap :: VarMap m,
    funcmap :: Map String (Function m)
}

newtype TigressT m a = TigressT { invTigress :: ExceptT String (StateT (TigState m) m) a } deriving (Functor, Applicative, Monad, MonadState (TigState m), MonadError String)

instance MonadTrans TigressT where
  lift = TigressT . lift . lift

-- value for tigress.

data Value = 
  VInt !Integer
  | VStr !String
  | VRec !(Map String Value)
  | VNil
  | VNone -- This shows that no values are returned.
  deriving (Eq, Show)
type VarRef m = MutVar (PrimState m)

type VarMap m = Map String (VarRef m Value)

data Function m = 
  Function ![String] !(VarRef m (TigState m)) !Expr

emptyTigState :: TigState m
emptyTigState = TigState Map.empty Map.empty

runTigress :: (PrimMonad m, Monad m) => TigressT m a -> m (Either String a)
runTigress m = evalStateT (runExceptT (invTigress m)) emptyTigState

newVariable :: (PrimMonad m, Monad m) => TigressT m (VarRef m Value)
newVariable = lift $ newMutVar VNone


-- | Saves current environment, performs action m and restore environment after action.
-- | Variables in original environment are modified if they are modified in m.
sandbox :: (PrimMonad m, Monad m) => TigressT m a -> TigressT m a
sandbox m = do
  s <- get
  result <- m
  put s
  return result

getVar :: (PrimMonad m, Monad m) => LValue -> TigressT m (VarRef m Value)
getVar (LId (Id name)) = do
   m <- liftM varmap get
   case Map.lookup name m of
     Just p  -> return p
     Nothing -> throwError $ "undefined variable: " ++ name
getVar (LMem _ _) = throwError "TODO: getVar-mem"
getVar (LIdx lv e) = throwError "TODO: getVar-index"

readVar :: (PrimMonad m, Monad m) => VarRef m a -> TigressT m a
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
eval (EApp (Id name) args) = do
  fm <- liftM funcmap get
  case Map.lookup name fm of
    Nothing -> throwError $ "undefined function: " ++ name
    Just (Function params tigState expr) -> do
      when (length params /= length args) (throwError $ "wrong number of arguments: " ++ show (length args) ++ " (expected: " ++ show (length params) ++ ")")
      env <- readVar tigState
      newenv <- foldM (\env (name, ex) -> do
        val <- eval ex
        newVar <- lift $ newMutVar val
        return $ env { varmap = Map.insert name newVar (varmap env) }
       ) env (zip params args)
      oldenv <- get
      put newenv
      result <- eval expr
      put oldenv
      return result
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
eval (EFor (Id name) from to expr) = do
  fromVal <- ensureInt =<< eval from
  toVal   <- ensureInt =<< eval to
  var     <- newVariable
  env <- get
  let newenv = env { varmap = Map.insert name var (varmap env) }
  forM_ [fromVal .. toVal] $ \i -> do
    updateVar var (VInt i)
    put newenv
    eval expr
  put env -- restore
  return VNone
eval EBreak       = throwError "TODO: break"
eval (ELet decs exprs) = sandbox $ do
  addDecs decs
  results <- mapM eval exprs
  return $ last (VNone : results)

-- | Processes declarations and modifies the variable environment.
-- | The original variable environment and function environment are modified.
addDecs :: (PrimMonad m, Monad m) => [Dec] -> TigressT m ()
addDecs decs = do
    newTigState <- lift . newMutVar =<< get
    forM_ decs (addDec newTigState) -- passes mutable environment in order to allow mutual recursions
    fm <- liftM funcmap $ lift (readMutVar newTigState)
    modify $ \s -> s { funcmap = fm }
    return ()

-- | Processes one declaration and modifies the variable environment.
-- | The original variable environment is modified, but function environment is NOT modified. 
addDec :: (PrimMonad m, Monad m) => VarRef m (TigState m) -> Dec -> TigressT m ()
addDec _newTigState (DType _) = return ()
addDec _newTigState (DVar (VarDec (Id id) ty expr)) = do
  var <- newVariable
  val <- eval expr
  updateVar var val
  vm <- liftM varmap get
  let newvm = Map.insert id var vm
  modify $ \s -> s { varmap = newvm }
  return ()
addDec newTigState (DFun (FunDec (Id name) params ty expr)) = do
    let fct = Function (map ( \(TypeField (Id x) _) -> x) params) newTigState expr
    fm <- liftM funcmap $ lift (readMutVar newTigState)
    let newfm = Map.insert name fct fm
    lift $ modifyMutVar newTigState $ \s -> s { funcmap = newfm } -- modifies newTigState, does not modify env in state
    return ()



