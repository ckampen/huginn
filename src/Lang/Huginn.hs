module Lang.Huginn ( module H ) where

-- import Control.Monad.State
-- import Control.Applicative

-- import Data.Maybe
import Lang.Huginn.AST as H
import Lang.Huginn.Eval as H
import Lang.Huginn.Parser as H
-- import Debug.Trace

-- type EnvEntry = (String, Expr)
-- type EnvEntries = [EnvEntry]
-- data Env = Env { vars :: EnvEntries, functions :: EnvEntries } deriving Show

-- type EvalExpr = (Expr, Env)

-- updateExp :: String -> EvalExpr -> EvalExpr
-- updateExp n (e,v) = (e, Env {vars = vr, functions = fn})
--   where vr = (n, e):vars v
--         fn = functions v

-- -- evalFn :: String -> Env -> EvalExpr
-- -- evalFn name env = updateExp name res
-- --   where res = eval env (fromMaybe (Err $ Unbound name) (lookup name (functions env)))

-- -- getVarValue :: String -> Env -> EvalExpr
-- -- getVarValue name env = case lookup name (vars env) of
-- --                             Just v -> (v, env)
-- --                             Nothing -> evalFn name env

-- getNumber :: Expr -> Expr
-- getNumber (Num fl) = Num fl
-- getNumber (Bl True) = Num 1.00
-- getNumber (Bl False) = Num 0.00
-- -- getNumber (Var v) = getNumber $ getVarValue v
-- getNumber _ = Err NaN

-- exprToValueTpl :: Expr -> (String, String)
-- exprToValueTpl (Num n) = ("float", show n)
-- exprToValueTpl (Str n) = ("string", show n)
-- exprToValueTpl (Bl True) = ("bool", "true")
-- exprToValueTpl (Bl False) = ("bool", "false")
-- exprToValueTpl _ = ("error", "Error")

-- exprToString :: Expr -> String
-- exprToString (Num n) = show n
-- exprToString (Str n) = show n
-- exprToString (Bl True) = "true"
-- exprToString (Bl False) = "false"
-- exprToString _ = "Error"

-- calc :: Operant -> Expr -> Expr -> Expr
-- calc Add a b = getNumber a `add` getNumber b
-- calc Mul a b = getNumber a `mul` getNumber b
-- calc Sub a b = getNumber a `sub` getNumber b
-- calc EQUALS a b = getNumber a `eq` getNumber b
-- calc POW a b = getNumber a `pow` getNumber b
-- calc Div a b = getNumber a `divExp` getNumber b
-- calc _ _ _ = Err Unsupported

-- mapEnvEntries :: (String, Either t Expr) -> EnvEntry
-- mapEnvEntries (n, Right ex) = (n, ex)
-- mapEnvEntries (n, Left _) = (n, Err Parse)

-- -- mapEval :: Env -> (String, Expr) -> (String, EvalExpr)
-- -- mapEval env (n, e) = (n, res)
-- --   where res = eval env e

-- -- -- mapEval' :: Env -> [(String, String)] -> [(String, String)]
-- -- mapEval' :: Env -> [(String, Expr)] -> [(String, String)]
-- -- mapEval' env ((name, fn):xs) = (name, exprToString res):mapEval' env' xs
-- --   where (res, env') = eval env fn
-- -- mapEval' _ [] = []

-- isTrue :: Expr -> Bool
-- isTrue (Bl True) = True
-- isTrue (Bl False) = False
-- isTrue _ = False

-- type EvalM = State Env Expr

-- getVarValues :: String -> Env -> Expr
-- getVarValues name env = fromMaybe (evalFns name env) (lookup name (vars env))
-- -- getVarValues name env = fromJust $ (lookup name (vars env)) <|> Just (evalFns name env)
-- -- getVarValues name env = case lookup name (vars env) of
-- --                             Just v -> v
-- --                             Nothing -> evalFns name env

-- evalFns :: String -> Env -> Expr
-- evalFns name env = evalState (evals (fromMaybe (Err $ Unbound name) (lookup name (functions env)))) env

-- runBinary op left right env = evalState (evals left) env `op` evalState (evals right) env

-- runEval :: Env -> Expr -> (Expr, Env)
-- runEval env e = runState (evals e) env
-- -- runEval env e = trace ("\n\n#########\n\n" ++ show e ++ "\n\n#########\n\n") $ runState (evals e) env

-- evals :: Expr -> EvalM
-- evals (Eeq left right) = runBinary eq left right <$> get
-- evals (Egt left right) = runBinary gt left right <$> get
-- evals (Elt left right) = runBinary lt left right <$> get
-- evals (Egte left right) = runBinary gte left right <$> get
-- evals (Elte left right) = runBinary lte left right <$> get
-- evals (Epow left right) = runBinary pow left right <$> get
-- evals (Eadd left right) = runBinary add left right <$> get
-- evals (Esub left right) = runBinary sub left right <$> get
-- evals (Emul left right) = runBinary mul left right <$> get
-- evals (Ediv left right) = runBinary divExp left right <$> get
-- evals (Var n) = fmap (getVarValues n) get
-- evals a@(Num _) = return a
-- evals b@(Bl _) = return b
-- evals (Let n v ex) = do
--   (val, env) <- runState (evals v) <$> get
--   -- put (env { vars = (n, val) : vars env })
--   return $ evalState (evals ex) (env { vars = (n, val) : vars env })
-- evals (Closure e) = evalState (evals e) <$> get
-- evals (If (test, left, right)) = do
--   (predicate, env) <- runState (evals test) <$> get
--   case predicate of
--        (Bl True) -> return $ evalState (evals left) env
--        (Bl False) -> return $ evalState (evals right) env
--        _ -> return $ Err Uncomparable
-- evals e@(Err _) = return e
-- evals e = return $ Err (Unimplemented (show e))

-- -- TODO: user StateT 
-- -- eval :: Env -> Expr -> EvalExpr
-- -- eval env (Eeq left right) = (left' `eq` right', env'')
-- --   where (left', env') = eval env left
-- --         (right', env'') = eval env' right

-- -- eval env (Egt left right) = (left' `gt` right', env'')
-- --   where (left', env') = eval env left
-- --         (right', env'') = eval env' right

-- -- eval env (Egte left right) = (left' `gte` right', env'')
-- --   where (left', env') = eval env left
-- --         (right', env'') = eval env' right

-- -- eval env (Elt left right) = (left' `lt` right', env'')
-- --   where (left', env') = eval env left
-- --         (right', env'') = eval env' right

-- -- eval env (Elte left right) = (left' `lte` right', env'')
-- --   where (left', env') = eval env left
-- --         (right', env'') = eval env' right

-- -- eval env (Epow left right) = (left' `pow` right', env'')
-- --   where (left', env') = eval env left
-- --         (right', env'') = eval env' right
-- -- eval env (Eadd left right) = (left' `add` right', env'')
-- --   where (left', env') = eval env left
-- --         (right', env'') = eval env' right
-- -- eval env (Esub left right) = (left' `sub` right', env'')
-- --   where (left', env') = eval env left
-- --         (right', env'') = eval env' right
-- -- eval env (Emul left right) = (left' `mul` right', env'')
-- --   where (left', env') = eval env left
-- --         (right', env'') = eval env' right
-- -- eval env (Ediv left right) = (left' `divExp` right', env'')
-- --   where (left', env') = eval env left
-- --         (right', env'') = eval env' right
-- -- eval env (If (test, left, right)) =
-- --   case predicate of
-- --        (Bl True) -> eval env' left
-- --        (Bl False) -> eval env' right
-- --        _ -> (Err Uncomparable, env')
-- --   where (predicate, env') = eval env test

-- -- eval env (Var n) = getVarValue n env
-- -- eval env (Closure e) = eval env e
-- -- eval env a@(Num _) = (a, env)
-- -- eval env b@(Bl _) = (b, env)
-- -- eval env (Let n v ex) = let val = eval env v in
-- --                           eval (env { vars = (n, fst val) : vars env }) ex
-- -- eval env ex = (ex, env) -- Err Unsupported

-- add :: Expr -> Expr -> Expr
-- add (Num a) (Num b) = Num (a + b)
-- add _ _ = Err NaN

-- sub :: Expr -> Expr -> Expr
-- sub (Num a) (Num b) = Num (a - b)
-- sub _ _ = Err NaN

-- mul :: Expr -> Expr -> Expr
-- mul (Num a) (Num b) = Num (a * b)
-- mul _ _ = Err NaN

-- pow :: Expr -> Expr -> Expr
-- pow (Num a) (Num b) = Num (a**b)
-- pow _ _ = Err NaN

-- eq :: Expr -> Expr -> Expr
-- eq (Num a) (Num b) = Bl (a == b)
-- eq (Bl a) (Bl b) = Bl (a == b)
-- eq (Str a) (Str b) = Bl (a == b)
-- eq _ _ = Err Uncomparable

-- gt :: Expr -> Expr -> Expr
-- gt (Num a) (Num b) = Bl (a > b)
-- gt (Str a) (Str b) = Bl (a > b)
-- gt _ _ = Err Uncomparable

-- gte :: Expr -> Expr -> Expr
-- gte (Num a) (Num b) = Bl (a >= b)
-- gte (Str a) (Str b) = Bl (a >= b)
-- gte _ _ = Err Uncomparable

-- lt :: Expr -> Expr -> Expr
-- lt (Num a) (Num b) = Bl (a < b)
-- lt (Str a) (Str b) = Bl (a < b)
-- lt _ _ = Err Uncomparable

-- lte :: Expr -> Expr -> Expr
-- lte (Num a) (Num b) = Bl (a <= b)
-- lte (Str a) (Str b) = Bl (a <= b)
-- lte _ _ = Err Uncomparable

-- divExp :: Expr -> Expr -> Expr
-- divExp (Num a) (Num b) = Num (a / b)
-- divExp _ _ = Err NaN
