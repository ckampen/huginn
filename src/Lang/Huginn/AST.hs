{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Lang.Huginn.AST where

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
          deriving (Show, Read)

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

