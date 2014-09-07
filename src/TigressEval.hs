{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TigressEval where

import TigressExpr
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Pos = Int -- the position of variable
type PosMap = Map String Pos


data TigState = TigState {
    posmap :: PosMap,
    varmap :: Map Pos Value,
    count  :: Int
} deriving Show

newtype TigressT m a = TigressT { invTigress :: ExceptT String (StateT TigState m) a } deriving (Functor, Applicative, Monad, MonadState TigState, MonadError String)

emptyTigState :: TigState
emptyTigState = TigState Map.empty Map.empty 0

runTigress :: Monad m => TigressT m a -> m (Either String a)
runTigress m = evalStateT (runExceptT (invTigress m)) emptyTigState

freshVariable :: Monad m => TigressT m Pos
freshVariable = do
  i <- liftM count $ get
  modify $ \s -> s { count = i + 1 }
  return i


getPos :: Monad m => LValue -> TigressT m Pos
getPos (LId (Id name)) = do
   m <- liftM posmap get
   case Map.lookup name m of
     Just p  -> return p
     Nothing -> throwError $ "undefined variable: " ++ name
getPos (LMem _ _) = throwError "TODO: getpos-mem"
getPos (LIdx lv e) = do
  iv <- ensureInt =<< eval e
  pos <- getPos lv
  return $ pos + fromIntegral iv


updateVar :: Monad m => Pos -> Value -> TigressT m ()
updateVar pos value = do
  vm <- liftM varmap get
  when (Map.notMember pos vm) $ throwError $ "attempt to write on invalid position: " ++ show pos  
  modify $ \s -> s {varmap = Map.insert pos value vm}

ensureInt :: Monad m => Value -> TigressT m Integer
ensureInt (VInt v) = return v
ensureInt value    = throwError $ "not an integer: " ++ show value

eval :: Monad m => Expr -> TigressT m Value
eval (EStr str) = return $ VStr str
eval (EInt i)   = return $ VInt i
eval ENil       = return VNil
eval (ELValue lv) = do
    pos <- getPos lv
    vm <- liftM varmap get
    case Map.lookup pos vm of
      Just p  -> return p
      Nothing -> throwError $ "invalid position: " ++ show pos
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
  pos <- getPos lv
  value <- eval e
  updateVar pos value
  return value
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

addDec :: Monad m => Dec -> TigressT m PosMap
addDec (DType _) = liftM posmap get
addDec (DVar (VarDec (Id id) ty expr)) = do
  pos <- freshVariable
  val <- eval expr
  updateVar pos val
  pm <- liftM posmap $ get
  let newpm = Map.insert id pos pm
  modify $ \s -> s { posmap = newpm }
  return newpm
addDec (DFun (FunDec id params ty expr)) = throwError "TODO: function declaration"



