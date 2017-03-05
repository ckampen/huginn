{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Lang.Huginn.AST where

import Control.Monad

  -- import Data.Text

data Error = NaN | DivByZero | Unbound String | Parse | Uncomparable | Unsupported | Unimplemented String  deriving (Show, Read)

data Operant = Mul | Div | Add | Sub | EQUALS | POW | Error String deriving (Show, Read)

data Expr = Uop (Operant, Expr)
          | Bop (Expr, Operant, Expr)
          | Num Double
          | Const Double
          | Str String
          | Bl Bool
          | If (Expr, Expr, Expr)
          | Var String
          | Arr [Expr]
          | Closure Expr
          | Err Error
          | Eeq Expr Expr
          | Epow Expr Expr
          | Emul Expr Expr
          | Ediv Expr Expr
          | Eadd Expr Expr
          | Esub Expr Expr
          | Elt Expr Expr
          | Elte Expr Expr
          | Egt Expr Expr
          | Egte Expr Expr
          | Let String Expr Expr
          | Fn String Expr Expr
          | Define String Expr
          | Fun String [(Expr, Expr)] Expr
          | Lambda String Expr Expr
          deriving (Show, Read)

data Expre a where
  NumE :: Double -> Expre Double
  AddE :: Expre Double -> Expre Double -> Expre Double
  SubE :: Expre Double -> Expre Double -> Expre Double
  MulE :: Expre Double -> Expre Double -> Expre Double
  DivE :: Expre Double -> Expre Double -> Expre Double
  PowE :: Expre Double -> Expre Double -> Expre Double
  EqE ::  Eq a => Expre a -> Expre a -> Expre Bool
  NeqE ::  Eq a => Expre a -> Expre a -> Expre Bool
  LtE :: Expre Double -> Expre Double -> Expre Bool
  LteE :: Expre Double -> Expre Double -> Expre Bool
  GtE :: Expre Double -> Expre Double -> Expre Bool
  GteE :: Expre Double -> Expre Double -> Expre Bool
  ArrE :: [a] -> Expre [a]
  ConstE :: Double -> Expre Double
  VarE :: String -> Expre Double
  StrE :: String -> Expre String
  BoolE :: Bool -> Expre Bool
  LetE :: String -> Double -> Expre a -> Expre a
  PrintE :: String -> Expre String

data EvalEM a = EvalEM { hEnv :: [(String, Double)]
                     , hRes :: a} deriving (Show)

instance Functor EvalEM where
  fmap fn (EvalEM env val) = EvalEM env (fn val)

instance Applicative EvalEM where
  pure = EvalEM []
  (<*>) = ap

instance Monad EvalEM where
  (EvalEM _ val) >>= fn = fn val



  -- data Lexpr = Lambda [Text] Lexpr
  --            | Llet [(Text, Lexpr)]
  --            | Ldefine Text [Text] Lexpr
  --            | Ldata Lexpr
  --            | Lstring Text
  --            | Lint Int
  --            | Ldouble Double


  -- data LispExpr a where
  --   Lambda :: [Text] -> LispExpr a -> LispExpr ([Text], LispExpr a)
  --   Llet :: [Text] -> LispExpr a -> LispExpr (Text, LispExpr a)
  --   Ldefine :: Text -> [Text] -> LispExpr a -> LispExpr  (Text, [Text], LispExpr a)
  --   Lint :: Int -> LispExpr Int
  --   Ldouble :: Double -> LispExpr Double
  --   Lstring :: Text -> LispExpr Text
  --   Ldata :: a -> LispExpr a

  -- evalLisp :: Lexpr -> Lexpr
  -- evalLisp (Lambda params body) = Ldata (Lambda params body)
  -- evalLisp (Ldata a) = a

