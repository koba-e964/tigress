{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TigressEval where

import TigressExpr
import Control.Applicative (Applicative)
import Control.Monad (foldM, forM, forM_, liftM, replicateM, when, (<=<))
import Control.Monad.Cont (ContT, MonadCont, callCC, runContT)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, get, modify, put)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Array -- (Array)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Primitive.MutVar (MutVar, modifyMutVar, newMutVar, readMutVar, writeMutVar)
import qualified Data.Traversable as DT
import Debug.Trace (trace)
import GHC.Arr (unsafeAt)

type Pos = Int -- the position of variable


data TigState r m = TigState {
    varmap :: VarMap m
    ,funcmap :: Map String (Function r m)
    ,tigCont :: Maybe (Value m -> TigressT r m (Value m))
}

-- | Configuration for IO
data TigConfig m = TigConfig {
    stdoutWriter :: String -> m ()
    ,stderrWriter :: String -> m ()
}

newtype TigressT r m a = TigressT { invTigress :: ExceptT String (ContT r (StateT (TigState r m) (ReaderT (TigConfig m) m))) a } deriving (Functor, Applicative, Monad, MonadState (TigState r m), MonadReader (TigConfig m), MonadError String, MonadCont)

instance MonadTrans (TigressT r) where
  lift = TigressT . lift . lift . lift . lift

-- value for tigress.
data Value m = 
  VInt !Integer
  | VStr !String
  | VRec !(Map String (Value m))
  | VArr !(Array Int (VarRef m (Value m)))
  | VNil
  | VNone -- This shows that no values are returned.
  deriving Eq

-- | Immutable value.
data FreezedValue =
  FVInt !Integer
  | FVStr !String
  | FVRec !(Map String FreezedValue)
  | FVArr !(Array Int FreezedValue)
  | FVNil
  | FVNone -- This shows that no values are returned.
  deriving Eq

instance Show FreezedValue where
  show (FVInt i) = show i
  show (FVStr s) = show s
  show (FVRec {}) = error "not supported: record"
  show (FVArr ary) =
    show (elems ary)    
  show FVNil     = "nil"
  show FVNone    = "(none)"
  

type VarRef m = MutVar (PrimState m)

type VarMap m = Map String (VarRef m (Value m))

data Function r m = 
  Function ![String] !(VarRef m (TigState r m)) !Expr
  | FunNative !Int {- argc -} !([Value m] -> TigressT r m (Value m)) {- body -} -- for native function

emptyTigState :: TigState r m
emptyTigState = TigState {varmap = Map.empty, funcmap = Map.empty, tigCont = Nothing}

stdlibTigState :: (PrimMonad m, Monad m) => TigState r m -- with standard library functions
stdlibTigState = TigState {varmap = Map.empty, funcmap = Map.fromList [("printi", FunNative 1 nativePrinti), ("not", FunNative 1 nativeNot)], tigCont = Nothing}


runTigress :: (PrimMonad m, Monad m) => TigConfig m -> TigressT (Either String a) m a -> m (Either String a)
runTigress conf m = runReaderT (evalStateT (runContT (runExceptT $ invTigress m) return) stdlibTigState) conf

runTigressExpr :: (PrimMonad m, Monad m) => TigConfig m -> Expr -> m (Either String FreezedValue)
runTigressExpr conf expr = runTigress conf $ do
  result <- eval expr
  freezeValue result

-- | Freezes a value. FreezedValue cannot be modified.
freezeValue :: (PrimMonad m, Monad m) => Value m -> TigressT r m FreezedValue
freezeValue (VStr str) = return (FVStr str)
freezeValue (VInt i)   = return (FVInt i)
freezeValue (VRec {})  = throwError "TODO: freeze-value record"
freezeValue (VArr ary) = liftM FVArr $ DT.mapM (freezeValue <=< readVar) ary
freezeValue VNil       = return FVNil
freezeValue VNone      = return FVNone

-- | emitWarning writes the given error message to stderrWriter.
-- | Newline is added after the given message (msg).
emitWarning :: (PrimMonad m, Monad m) => String -> TigressT r m ()
emitWarning msg = do
  writer <- asks stderrWriter
  lift (writer (msg ++ "\n"))
newVariable :: (PrimMonad m, Monad m) => TigressT r m (VarRef m (Value m))
newVariable = lift $ newMutVar VNone


-- | Saves current environment, performs action m and restore environment after action.
-- | Variables in original environment are modified if they are modified in m.
sandbox :: (PrimMonad m, Monad m) => TigressT r m a -> TigressT r m a
sandbox m = do
  s <- get
  result <- m
  put s
  return result

getVar :: (PrimMonad m, Monad m) => LValue -> TigressT r m (VarRef m (Value m))
getVar (LId (Id name)) = do
   m <- liftM varmap get
   case Map.lookup name m of
     Just p  -> return p
     Nothing -> throwError $ "undefined variable: " ++ name
getVar (LMem _ _) = throwError "TODO: getVar-mem"
getVar (LIdx lv e) = do {
  aryOrErr <- getVar lv >>= readVar;
  case aryOrErr of {
    VArr ary -> do {
      index <- liftM fromIntegral $ ensureInt =<< eval e;
      let {length = rangeSize $ bounds ary;};
      when (index < 0 || index >= length) $ throwError $ "array index out of bounds: " ++ show index ++ " (length=" ++ show length ++ ")";
      return (unsafeAt ary index);
    };
    _        -> throwError "array expected";
  };
}
readVar :: (PrimMonad m, Monad m) => VarRef m a -> TigressT r m a
readVar var = lift $ readMutVar var
updateVar :: (PrimMonad m, Monad m) => VarRef m (Value m) -> Value m -> TigressT r m ()
updateVar var value = lift $ writeMutVar var value

ensureInt :: (PrimMonad m, Monad m) => Value m -> TigressT r m Integer
ensureInt (VInt v) = return v
ensureInt value    = throwError =<< liftM ("not an integer: " ++) (showValue value)

eval :: (PrimMonad m, Monad m) => Expr -> TigressT r m (Value m)
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
eval (EBin op e1 e2) =
  case op of
    BAnd -> iand e1 e2
    BOr  -> ior e1 e2
    BDiv -> idiv e1 e2
    _    -> do
      i1 <- ensureInt =<< eval e1
      i2 <- ensureInt =<< eval e2
      let ops = [(BAdd,(+)), (BSub,(-)), (BMul,(*)), (BEq,bToI (==)), (BNeq,bToI (/=)), (BGt,bToI (>)), (BLt,bToI (<)),
           (BGe,bToI (>=)), (BLe,bToI (<=))] :: [(BinOp, Integer->Integer->Integer)]
      let opInt = fromJust (lookup op ops)
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
      sandbox $ do
        env <- readVar tigState
        newenv <- foldM (\env (name, ex) -> do
          val <- eval ex
          newVar <- lift $ newMutVar val
          return $ env { varmap = Map.insert name newVar (varmap env) }
         ) env (zip params args)
        put newenv
        eval expr
    Just (FunNative argc body) -> do
      when (argc /= length args) (throwError $ "wrong number of arguments: " ++ show (length args) ++ " (expected: " ++ show argc ++ ")")
      argsVal <- mapM eval args
      body argsVal
eval (ESeq ls) = do
  results <- forM ls eval
  return $ last (VNone : results)
eval (ERec {}) = throwError "TODO: record"
eval (EArr _typeid length initial) = do { -- ignores type-id and creates generic array. type is not checked in this interpreter.
  initVal <- eval initial;
  lenVal  <- liftM fromIntegral $ ensureInt =<< eval length;
  ary <- liftM (listArray (0,lenVal-1)) $ replicateM lenVal (lift (newMutVar initVal));
  return $ VArr ary;
}
eval (EIf cond e1) = do
  b <- ensureInt =<< eval cond
  when (b /= 0) $ do -- a nonzero value is regarded to be true.
     result <- eval e1
     when (result /= VNone) $ emitWarning . (++) "in if-then, then clause should not return a value, but returned " =<< showValue result
     return ()
  return VNone -- always returns no value.
eval (EIfElse cond e1 e2) = do
  b <- ensureInt =<< eval cond
  case b of
    0 -> eval e2 -- 0 is regarded to be false.
    _ -> eval e1 -- otherwise, an integer is regarded to be true.
eval (EWhile cond expr) = loopContext go where
  go = do
    condI <- ensureInt =<< eval cond
    case condI of
      0 -> return VNone
      _ -> do
        result <- eval expr
        when (result /= VNone) $ (emitWarning . ("expr in while must not return a value, but returned a " ++)) =<< showValue result
        go
eval (EFor (Id name) from to expr) = loopContext go where
  go = do
    fromVal <- ensureInt =<< eval from
    toVal   <- ensureInt =<< eval to
    var     <- newVariable
    modify $ \env -> env { varmap = Map.insert name var (varmap env) }
    forM_ [fromVal .. toVal] $ \i -> do
      updateVar var (VInt i)
      result <- eval expr
      when (result /= VNone) $ (emitWarning . ("expr in for must not return a value, but returned a " ++)) =<< showValue result
    return VNone
eval EBreak       = do
  maybeCont <- liftM tigCont get
  case maybeCont of
    Just cont -> cont VNone
    Nothing   -> throwError "Not in breakable context"
eval (ELet decs exprs) = sandbox $ do
  addDecs decs
  results <- mapM eval exprs
  return $ last (VNone : results)

-- | Executes go in loop. If go calls break during execution, the execution of go will be terminated and the control goes back to the
-- | outside of loopContext.
loopContext :: (PrimMonad m, Monad m) => TigressT r m (Value m) -> TigressT r m (Value m)
loopContext go = sandbox $ callCC $ \cont -> do
  modify $ \s -> s { tigCont = Just cont }
  go

-- | Processes declarations and modifies the variable environment.
-- | The original variable environment and function environment are modified.
addDecs :: (PrimMonad m, Monad m) => [Dec] -> TigressT r m ()
addDecs decs = do
    newTigState <- lift . newMutVar =<< get
    forM_ decs (addDec newTigState) -- passes mutable environment in order to allow mutual recursions
    fm <- liftM funcmap $ lift (readMutVar newTigState)
    modify $ \s -> s { funcmap = fm }
    return ()

-- | Processes one declaration and modifies the variable environment.
-- | The original variable environment is modified, but function environment is NOT modified. 
addDec :: (PrimMonad m, Monad m) => VarRef m (TigState r m) -> Dec -> TigressT r m ()
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

showValue :: (PrimMonad m, Monad m) => Value m -> TigressT r m String
showValue (VStr str) = return str
showValue (VInt i)   = return (show i)
showValue (VRec {})  = throwError "TODO: show record"
showValue (VArr {})  = throwError "TODO: show array"
showValue VNil       = return "nil"
showValue VNone      = return "(NONE)"


{-
  Functions that implements functions in standard library.
    function printi(i : int)
    function not(i : int) : int
-}

nativePrinti :: (PrimMonad m, Monad m) => [Value m] -> TigressT r m (Value m)
nativePrinti [val] = do
  ival <- ensureInt val
  writer <- asks stdoutWriter
  lift $ writer $ show ival
  return VNone
nativePrinti _ = undefined

nativeNot :: (PrimMonad m, Monad m) => [Value m] -> TigressT r m (Value m)
nativeNot [val] = do
  ival <- ensureInt val
  return $ VInt $ if ival == 0 then 1 else 0
