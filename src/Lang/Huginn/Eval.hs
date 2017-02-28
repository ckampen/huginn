{-# LANGUAGE GADTs #-}
module Lang.Huginn.Eval where

import Control.Monad.State
-- import Control.Applicative

import Data.Maybe
import Lang.Huginn.AST
-- import Debug.Trace

type EnvEntry = (String, Expr)
type EnvEntries = [EnvEntry]
data Env = Env { vars :: EnvEntries, functions :: EnvEntries } deriving Show

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

calc :: Operant -> Expr -> Expr -> Expr
calc Add a b = getNumber a `add` getNumber b
calc Mul a b = getNumber a `mul` getNumber b
calc Sub a b = getNumber a `sub` getNumber b
calc EQUALS a b = getNumber a `eq` getNumber b
calc POW a b = getNumber a `pow` getNumber b
calc Div a b = getNumber a `divExp` getNumber b
calc _ _ _ = Err Unsupported

mapEnvEntries :: (String, Either t Expr) -> EnvEntry
mapEnvEntries (n, Right ex) = (n, ex)
mapEnvEntries (n, Left _) = (n, Err Parse)

isTrue :: Expr -> Bool
isTrue (Bl True) = True
isTrue (Bl False) = False
isTrue _ = False

type EvalM = State Env Expr

-- evalEvalM :: State s a -> s -> a
evalEvalM :: EvalM -> Env -> Expr
evalEvalM = evalState

-- runEvalM :: State s a -> s -> (a, s)
runEvalM :: EvalM -> Env -> (Expr, Env)
runEvalM = runState

type EvalEMS a = State Env a
evalEvalEMS :: EvalEMS a -> Env -> a
evalEvalEMS = evalState

runEvalEMS :: EvalEMS a -> Env -> (a, Env)
runEvalEMS = runState

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
evals (Eeq left right) = runBinary eq left right <$> get
evals (Egt left right) = runBinary gt left right <$> get
evals (Elt left right) = runBinary lt left right <$> get
evals (Egte left right) = runBinary gte left right <$> get
evals (Elte left right) = runBinary lte left right <$> get
evals (Epow left right) = runBinary pow left right <$> get
evals (Eadd left right) = runBinary add left right <$> get
evals (Esub left right) = runBinary sub left right <$> get
evals (Emul left right) = runBinary mul left right <$> get
evals (Ediv left right) = runBinary divExp left right <$> get
evals (Var n) = fmap (getVarValues n) get
evals a@(Num _) = return a
evals b@(Bl _) = return b
evals (Let n v ex) = do
  (val, env) <- runEvalM (evals v) <$> get
  -- put (env { vars = (n, val) : vars env })
  return $ evalEvalM (evals ex) (env { vars = (n, val) : vars env })
evals (Closure e) = evalEvalM (evals e) <$> get
evals (If (test, left, right)) = do
  (predicate, env) <- runEvalM (evals test) <$> get
  case predicate of
       (Bl True) -> return $ evalEvalM (evals left) env
       (Bl False) -> return $ evalEvalM (evals right) env
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

evalE :: Expre a -> EvalEMS a
evalE (NumE n)   = return n
evalE (BoolE b)  = return b
evalE (StrE s)   = return s
evalE (AddE a b) = do
  l <- evalEvalEMS (evalE a) <$> get
  r <- evalEvalEMS (evalE b) <$> get
  return (l + r)
-- evalE (SubE a b) = evalE a - evalE b
-- evalE (MulE a b) = evalE a * evalE b
-- evalE (DivE a b) = evalE a / evalE b
-- evalE (PowE a b) = evalE a ** evalE b
-- evalE (LtE a b)  = evalE a < evalE b
-- evalE (LteE a b) = evalE a <= evalE b
-- evalE (GtE a b)  = evalE a > evalE b
-- evalE (GteE a b) = evalE a >= evalE b
-- evalE (EqE a b)  = evalE a == evalE b
-- evalE (NeqE a b) = evalE a /= evalE b

