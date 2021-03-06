{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codegen where

import Data.Word (Word)
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map as Map
import Control.Monad (when)
import Control.Monad.State (MonadState, StateT, execStateT, gets, modify, runStateT)
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)

import LLVM.General.AST (Definition(..), FloatingPointFormat(..), Instruction(..), Name(..), Named(..), Operand(..), Terminator(..), Type(..), defaultModule, moduleDefinitions, moduleName)
import LLVM.General.AST.Global (BasicBlock(BasicBlock), Parameter(Parameter), basicBlocks, functionDefaults, name, parameters, returnType)
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.AddrSpace as AS
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.Linkage as Linkage
import LLVM.General.Context (Context, withContext)

{- Reference: https://github.com/sdiehl/kaleidoscope -}

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM { unLLVM :: StateT AST.Module (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module, MonadError String)

runLLVM :: AST.Module -> LLVM a -> Either String AST.Module
runLLVM astmod llvm = execStateT (unLLVM llvm) astmod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  AST.Type -> String -> [(AST.Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , G.linkage   = if label == "main" then Linkage.External else Linkage.Private
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  , G.functionAttributes = [A.InlineHint]
  }

external ::  AST.Type -> String -> [(AST.Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- IEEE 754 double
double :: AST.Type
double = FloatingPointType 64 IEEE

-- 1-bit integer (boolean)
int1 :: AST.Type
int1 = IntegerType 1

-- 8-bit integer (char)
int8 :: AST.Type
int8 = IntegerType 8

-- 32-bit integer
int32 :: AST.Type
int32 = IntegerType 32

-- 64-bit integer
int64 :: AST.Type
int64 = IntegerType 64


ptrType :: AST.Type -> AST.Type
ptrType ty = AST.PointerType ty (AS.AddrSpace 0)

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  , loopExits    :: [Name]                   -- loop exit info
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { unCodegen :: StateT CodegenState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadError String )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty []

execCodegen :: Codegen a -> Either String CodegenState
execCodegen m = execStateT (unCodegen m) emptyCodegen

runCodegen :: Codegen a -> Either String (a, CodegenState)
runCodegen m = runStateT (unCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen ()
setBlock bname =
  modify $ \s -> s { currentBlock = bname }

getBlock :: Codegen Name
getBlock = gets currentBlock

setLoopExits :: [Name] -> Codegen ()
setLoopExits bname =
  modify $ \s -> s { loopExits = bname }

getLoopExits :: Codegen [Name]
getLoopExits = gets loopExits

pushLoopExit :: Name -> Codegen ()
pushLoopExit bname =
   modify $ \s -> s { loopExits = bname : loopExits s }

popLoopExit :: Codegen (Maybe Name)
popLoopExit = do
  result <- gets loopExits
  case result of
    []    -> return Nothing
    (x:rest) -> do
      setLoopExits rest
      return (Just x)
modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> throwError $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = (var, x) : lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> throwError $ "Local variable not in scope: " ++ show var


-------------------------------------------------------------------------------
--- Types
-------------------------------------------------------------------------------

type TypedOperand = (AST.Type, AST.Operand)

checkType :: AST.Type -> TypedOperand -> Codegen AST.Operand
checkType ty (realTy, realVal) = do
  when (ty /= realTy) (throwError $ "Type error (expected: " ++ show ty ++ ", actual: " ++ show realTy ++ ")")
  return realVal

voidValue :: TypedOperand
voidValue = (AST.VoidType, cons $ C.Undef AST.VoidType)


-- References
local ::  Name -> Operand
local = LocalReference int64

global ::  Name -> C.Constant
global = C.GlobalReference int64

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

-- Arithmetic and Constants
add :: Operand -> Operand -> Codegen Operand
add a b = instr $ Add False False a b []

sub :: Operand -> Operand -> Codegen Operand
sub a b = instr $ Sub False False a b []

-- negation (i.e. -a)
neg :: Operand -> Codegen Operand
neg a = sub (cons (C.Int 64 0)) a

mul :: Operand -> Operand -> Codegen Operand
mul a b = instr $ Mul False False a b []

div :: Operand -> Operand -> Codegen Operand
div a b = do
  ccond <- gt a (cons $ C.Int 64 0)
  let onPos = do
        res <-instr $ SDiv False a b []
        return (int64, res)
  let onNeg = do
        bm1 <- sub b (cons (C.Int 64 1))
        ceil <- sub a bm1 -- a - b + 1
        res <- instr $ SDiv False ceil b []
        return (int64, res) -- (a - b + 1) / b is floor(a / b)
  conditional (return (int64, ccond)) onPos onNeg >>= checkType int64

cmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
cmp cond a b =
  boolToInt64 =<< instr (ICmp cond a b [])

eq :: AST.Operand -> AST.Operand -> Codegen AST.Operand
neq :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
gt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
le :: AST.Operand -> AST.Operand -> Codegen AST.Operand
ge :: AST.Operand -> AST.Operand -> Codegen AST.Operand

eq = cmp IP.EQ
neq = cmp IP.NE
lt = cmp IP.SLT
gt = cmp IP.SGT
le = cmp IP.SLE
ge = cmp IP.SGE


conditional :: Codegen TypedOperand -> Codegen TypedOperand -> Codegen TypedOperand -> Codegen TypedOperand
conditional cond etr efl = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  -- branch
  ccond <- cond >>= checkType int64
  test <- cmp IP.EQ ccond (cons $ C.Int 64 0)
  _ <- cbr test ifelse ifthen -- Branch based on the condition
  -- if.then
  ------------------
  setBlock ifthen
  trval <- etr       -- Generate code for the true branch
  _ <- br ifexit                -- Branch to the merge block
  ifthenEnd <- getBlock
  -- if.else
  ------------------
  setBlock ifelse
  flval <- efl       -- Generate code for the true branch
  _ <- br ifexit                -- Branch to the merge block
  ifelseEnd <- getBlock
  -- if.exit
  ------------------
  setBlock ifexit
  phi [(trval, ifthenEnd), (flval, ifelseEnd)]

-- | cond must be of type i1.
conditionalBool :: Codegen TypedOperand -> Codegen TypedOperand -> Codegen TypedOperand -> Codegen TypedOperand
conditionalBool cond etr efl = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  -- branch
  ccond <- cond >>= checkType int1
  _ <- cbr ccond ifelse ifthen -- Branch based on the condition
  -- if.then
  ------------------
  setBlock ifthen
  trval <- etr       -- Generate code for the true branch
  _ <- br ifexit                -- Branch to the merge block
  ifthenEnd <- getBlock
  -- if.else
  ------------------
  setBlock ifelse
  flval <- efl       -- Generate code for the true branch
  _ <- br ifexit                -- Branch to the merge block
  ifelseEnd <- getBlock
  -- if.exit
  ------------------
  setBlock ifexit
  phi [(trval, ifthenEnd), (flval, ifelseEnd)]

not0 :: TypedOperand -> Codegen TypedOperand
not0 op = conditional (return op) (return (int64, cons $ C.Int 64 1)) (return (int64, cons $ C.Int 64 0))

sand :: Codegen TypedOperand -> Codegen TypedOperand -> Codegen TypedOperand -- shortcut and
sor  :: Codegen TypedOperand -> Codegen TypedOperand -> Codegen TypedOperand  -- shortcut or

sand a b =
  conditional a (b >>= not0) (return (int64, cons $ C.Int 64 0))
    
sor a b = 
  conditional a (return (int64, cons $ C.Int 64 1)) (b >>= not0)



-- | zero-extension from boolean(i1) to int64
-- | false -> 0, true -> 1
boolToInt64 :: Operand -> Codegen Operand
boolToInt64 operand = instr $ ZExt operand int64 []

-- | Truncation from int64 to boolean(i1).
int64ToBool :: Operand -> Codegen Operand
int64ToBool operand = instr $ Trunc operand int1 [] 

cons :: C.Constant -> Operand
cons = ConstantOperand

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen (AST.Type, Operand)
call fn args = do
  val <- instr $ Call False CC.C [] (Right fn) (toArgs args) [] []
  return (int64, val) -- TODO support arbitrary type

alloca :: AST.Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = do
  condBool <- int64ToBool cond
  terminator $ Do $ CondBr condBool tr fl []

phi :: [((AST.Type, Operand), Name)] -> Codegen (AST.Type, Operand)
phi incoming = do
  when (null incoming) $ throwError $ "null phi node: " ++ show incoming
  let types = map (fst . fst) incoming
  when (any (/= head types) types) $ throwError $ "invalid phi node (type error):" ++ show incoming
  let ty = head types
  result <- instr $ Phi ty (map ( \((_ty, operand), nodeName) -> (operand, nodeName)) incoming) []
  return (ty, result)

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

retNone :: Codegen (Named Terminator)
retNone = terminator $ Do $ Ret Nothing []


----
withContextT :: (Context -> ExceptT e IO a) -> ExceptT e IO a
withContextT action = liftIO (withContext (runExceptT . action)) >>= liftEither

liftEither :: MonadError e m => Either e a -> m a
liftEither (Left e) = throwError e
liftEither (Right val) = return val



