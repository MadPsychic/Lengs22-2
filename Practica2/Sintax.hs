module Sintax where

type Identifier = String

data Expr = Var Identifier | I Int | B Bool
          | Fn Identifier Expr
          | Succ Expr | Pred Expr
          | Add Expr Expr | Mul Expr Expr
          | Not Expr | Iszero Expr
          | And Expr Expr | Or Expr Expr
          | Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
          | If Expr Expr Expr
          | Let Identifier Expr Expr
          | App Expr Expr
          deriving (Show, Eq)

