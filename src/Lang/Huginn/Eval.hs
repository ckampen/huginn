{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lang.Huginn.Eval where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Exception
-- import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.STM
import Control.Applicative


import Data.Maybe
import Data.List as L
import Lang.Huginn.AST
-- import Debug.Trace

type EnvEntry = (String, Expr)
type EnvEntries = [EnvEntry]
data Env = Env { vars :: EnvEntries, functions :: EnvEntries } deriving Show

newtype EvalRT a = MkEval (StateT [Env] IO a)
  deriving (Functor, Applicative, Monad, MonadIO,  MonadState [Env])

newEnv :: IO (TVar [Env])
newEnv = atomically $ newTVar []

runEvalRT :: EvalRT a -> IO (a, [Env])
runEvalRT (MkEval state) = runStateT state [emptyEnv]

runEvalx :: Expr -> IO (Expr, [Env])
runEvalx e = runEvalRT (evalx e)

lookupNamex :: String -> EvalRT (Maybe Expr)
lookupNamex name = do
  var <- lookupVarx name
  case var of
    Nothing -> lookupFnx name
    x -> return x

lookupVarx :: String -> EvalRT (Maybe Expr)
lookupVarx name = findInEnvs vars name <$> get

lookupFnx :: String -> EvalRT (Maybe Expr)
lookupFnx name = findInEnvs functions name <$> get

findInEnvs :: (Env -> EnvEntries) -> String -> [Env] -> Maybe Expr
findInEnvs _ _ [] = Nothing
findInEnvs fn name (x:xs) = lookup name (fn x) <|> findInEnvs fn name xs

evalx :: Expr -> EvalRT Expr
evalx (Eeq left right) = eq <$> evalx left <*> evalx right
evalx (Egt left right) = gt <$> evalx left <*> evalx right
evalx (Elt left right) = lt <$> evalx left <*> evalx right
evalx (Egte left right) = gte <$> evalx left <*> evalx right
evalx (Elte left right) = lte <$> evalx left <*> evalx right
evalx (Epow left right) = pow <$> evalx left <*> evalx right
evalx (Eadd left right) = add <$> evalx left <*> evalx right
evalx (Esub left right) = sub <$> evalx left <*> evalx right
evalx (Emul left right) = mul <$> evalx left <*> evalx right
evalx (Ediv left right) = divExp <$> evalx left <*> evalx right
evalx (Var n) = do
  val <- lookupNamex n
  case val of
    Nothing -> return (Err (Unbound n))
    (Just v) -> return v
evalx a@(Num _) = return a
evalx b@(Bl _) = return b
evalx (Arr xs) = Arr <$> mapM evalx xs
evalx (Let n v ex) = do
  val <- evalx v
  modify (\(env:xs) -> (env { vars = (n, val) : vars env }):xs)
  evalx ex
evalx (Fn n v ex) = do
  val <- evalx v
  modify (\(env:xs) -> (env { functions = (n, val) : functions env }):xs)
  evalx ex
evalx (Closure e) = evalx e
evalx (If (test, left, right)) = do
  predicate <- evalx test
  case predicate of
       (Bl True) -> evalx left
       (Bl False) -> evalx right
       _ -> return $ Err Uncomparable
evalx (Closure exp) = do
  modify (\envs -> emptyEnv : envs)
  evalx exp
evalx (Lambda p exp v) = do
  val <- evalx v
  modify (\envs -> (emptyEnv { vars = [(p, val)] }) : envs)
  evalx exp
evalx (Define n v) = do
  liftIO $ print "define"
  val <- evalx v
  modify (\(env:xs) -> (env { vars = (n, val) : vars env }):xs)
  return val

evalx e@(Err _) = return e
evalx e = return $ Err (Unimplemented (show e))

emptyEnv :: Env
emptyEnv = Env [] []


type EvalExpr = (Expr, Env)

updateExp :: String -> EvalExpr -> EvalExpr
updateExp n (e,v) = (e, Env {vars = vr, functions = fn})
  where vr = (n, e):vars v
        fn = functions v

getNumber :: Expr -> Expr
getNumber (Num fl) = Num fl
getNumber (Bl True) = Num 1.00
getNumber (Bl False) = Num 0.00
-- getNumber (Var v) = getNumber $ getVarValue v
getNumber _ = Err NaN

exprToValueTpl :: Expr -> (String, String)
exprToValueTpl (Num n) = ("float", show n)
exprToValueTpl (Str n) = ("string", show n)
exprToValueTpl (Bl True) = ("bool", "true")
exprToValueTpl (Bl False) = ("bool", "false")
exprToValueTpl _ = ("error", "Error")

exprToString :: Expr -> String
exprToString (Num n) = show n
exprToString (Str n) = show n
exprToString (Bl True) = "true"
exprToString (Bl False) = "false"
exprToString _ = "Error"

-- calc :: Operant -> Expr -> Expr -> Expr
-- calc Add a b = getNumber a `add` getNumber b
-- calc Mul a b = getNumber a `mul` getNumber b
-- calc Sub a b = getNumber a `sub` getNumber b
-- calc EQUALS a b = getNumber a `eq` getNumber b
-- calc POW a b = getNumber a `pow` getNumber b
-- calc Div a b = getNumber a `divExp` getNumber b
-- calc _ _ _ = Err Unsupported

mapEnvEntries :: (String, Either t Expr) -> EnvEntry
mapEnvEntries (n, Right ex) = (n, ex)
mapEnvEntries (n, Left _) = (n, Err Parse)

isTrue :: Expr -> Bool
isTrue (Bl True) = True
isTrue (Bl False) = False
isTrue _ = False

type EvalM = State Env Expr
-- type EvalM = ReaderT Env IO Expr

-- type EvalEM a = State EnvNum Expre a

-- evalEvalM :: State s a -> s -> a
evalEvalM :: EvalM -> Env -> Expr
evalEvalM = evalState

-- runEvalM :: State s a -> s -> (a, s)
runEvalM :: EvalM -> Env -> (Expr, Env)
runEvalM = runState

-- lookupName :: String -> EvalM
-- lookupName name = choose <$> lookupFn name <*> lookupVar name
--   where choose (Err _) x = x
--         choose x _ = x

lookupName :: String -> EvalM
lookupName name = do
  var <- lookupVar name
  case var of
    (Err _) -> lookupFn name
    x -> return x

lookupVar :: String -> EvalM
lookupVar name = do
  env <- get
  return $ fromMaybe (Err (Unbound name)) (lookup name (vars env))

lookupFn :: String -> EvalM
lookupFn name = do
  env <- get
  return $ fromMaybe (Err (Unbound name)) (lookup name (functions env))

getVarValues :: String -> Env -> Expr
getVarValues name env = fromMaybe (evalFns name env) (lookup name (vars env))

evalFns :: String -> Env -> Expr
evalFns name env = evalEvalM (evals (fromMaybe (Err $ Unbound name) (lookup name (functions env)))) env

runBinary :: (Expr -> Expr -> a) -> Expr -> Expr -> Env -> a
runBinary op left right env = evalEvalM (evals left) env `op` evalEvalM (evals right) env

runEval :: Env -> Expr -> (Expr, Env)
runEval env e = runEvalM (evals e) env
-- runEval env e = trace ("\n\n#########\n\n" ++ show e ++ "\n\n#########\n\n") $ runEvalM (evals e) env

evals :: Expr -> EvalM
evals (Eeq left right) = eq <$> evals left <*> evals right
evals (Egt left right) = gt <$> evals left <*> evals right
evals (Elt left right) = lt <$> evals left <*> evals right
evals (Egte left right) = gte <$> evals left <*> evals right
evals (Elte left right) = lte <$> evals left <*> evals right
evals (Epow left right) = pow <$> evals left <*> evals right
evals (Eadd left right) = add <$> evals left <*> evals right
evals (Esub left right) = sub <$> evals left <*> evals right
evals (Emul left right) = mul <$> evals left <*> evals right
evals (Ediv left right) = divExp <$> evals left <*> evals right
evals (Var n) = lookupName n
evals a@(Num _) = return a
evals b@(Bl _) = return b
evals (Arr xs) = Arr <$> mapM evals xs
evals (Let n v ex) = do
  val <- evals v
  modify (\env -> env { vars = (n, val) : vars env })
  evals ex
evals (Closure e) = evals e
evals (If (test, left, right)) = do
  predicate <- evals test
  case predicate of
       (Bl True) -> evals left
       (Bl False) -> evals right
       _ -> return $ Err Uncomparable
evals e@(Err _) = return e
evals e = return $ Err (Unimplemented (show e))

add :: Expr -> Expr -> Expr
add (Num a) (Num b) = Num (a + b)
add _ _ = Err NaN

sub :: Expr -> Expr -> Expr
sub (Num a) (Num b) = Num (a - b)
sub _ _ = Err NaN

mul :: Expr -> Expr -> Expr
mul (Num a) (Num b) = Num (a * b)
mul _ _ = Err NaN

pow :: Expr -> Expr -> Expr
pow (Num a) (Num b) = Num (a**b)
pow _ _ = Err NaN

eq :: Expr -> Expr -> Expr
eq (Num a) (Num b) = Bl (a == b)
eq (Bl a) (Bl b) = Bl (a == b)
eq (Str a) (Str b) = Bl (a == b)
eq _ _ = Err Uncomparable

gt :: Expr -> Expr -> Expr
gt (Num a) (Num b) = Bl (a > b)
gt (Str a) (Str b) = Bl (a > b)
gt _ _ = Err Uncomparable

gte :: Expr -> Expr -> Expr
gte (Num a) (Num b) = Bl (a >= b)
gte (Str a) (Str b) = Bl (a >= b)
gte _ _ = Err Uncomparable

lt :: Expr -> Expr -> Expr
lt (Num a) (Num b) = Bl (a < b)
lt (Str a) (Str b) = Bl (a < b)
lt _ _ = Err Uncomparable

lte :: Expr -> Expr -> Expr
lte (Num a) (Num b) = Bl (a <= b)
lte (Str a) (Str b) = Bl (a <= b)
lte _ _ = Err Uncomparable

divExp :: Expr -> Expr -> Expr
divExp (Num a) (Num b) = Num (a / b)
divExp _ _ = Err NaN

data EnvE a = EnvE [(String, Expre a)] -- deriving Show
data EnvNum = EnvNum [(String, Double)] -- deriving Show

type EvalEMS a = State EnvNum a

type EvalT a = StateT EnvNum IO a


data EnvTree = EnvTreeLeaf [(String, Expr)] | EnvTreeNode [(String, Expr)] [EnvTree]

data EnvironMent a = EnvironMent { envx :: TVar EnvTree
                               , callStackx :: TVar [Expre a]
}

evalT :: EvalT a -> EnvNum -> IO a
evalT = evalStateT

runT :: EvalT a -> EnvNum -> IO (a, EnvNum)
runT = runStateT

evalEvalEMS :: EvalEMS a -> EnvNum -> a
evalEvalEMS = evalState

runEvalEMS :: EvalEMS a -> EnvNum -> (a, EnvNum)
runEvalEMS = runState

evalE :: Expre a -> EvalT a
evalE (NumE n)   = return n
evalE (BoolE b)  = return b
evalE (StrE s)   = return s
evalE (AddE a b) = ( + )  <$> evalE a <*> evalE b
evalE (SubE a b) = ( - )  <$> evalE a <*> evalE b
evalE (MulE a b) = ( * )  <$> evalE a <*> evalE b
evalE (DivE a b) = ( * )  <$> evalE a <*> evalE b
evalE (PowE a b) = ( ** ) <$> evalE a <*> evalE b
evalE (LtE a b)  = ( < )  <$> evalE a <*> evalE b
evalE (LteE a b) = ( <= )  <$> evalE a <*> evalE b
evalE (GtE a b)  = ( > )  <$> evalE a <*> evalE b
evalE (GteE a b) = ( >= )  <$> evalE a <*> evalE b
evalE (EqE a b)  = ( == )  <$> evalE a <*> evalE b
evalE (NeqE a b) = ( /= )  <$> evalE a <*> evalE b
evalE (LetE n v ex) = modify (\(EnvNum env) -> EnvNum ((n, v) : env)) >> evalE ex
evalE (VarE n) = do
  (EnvNum env) <- get
  case  L.lookup n env of
    (Just v) -> return v
    Nothing -> error "Not bound"
evalE (PrintE s) = liftIO (putStrLn s) >> return s

test = LetE "x" 1.0 (AddE (VarE "x") (NumE 2.0))
test2 = evalE test

