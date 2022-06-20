module Sintax where

type Identifier = String

data Expr = Var Identifier | I Int | B Bool
          | Add Expr Expr | Mul Expr Expr
          | Succ Expr | Pred Expr
          | And Expr Expr | Or Expr Expr
          | Not Expr | Iszero Expr
          | Lt Expr Expr -- Menor que
          | Gt Expr Expr -- Mayor que
          | Eq Expr Expr -- Igual
          | If Expr Expr Expr
          | Let Identifier Expr Expr
          | App Expr Expr
          | Fn Identifier Expr
          | Raise Expr
          | Handle Expr Identifier Expr
-- ************** No las usaremos ***************

          | L Int
          | Void
          | While Expr Expr
          | Abs Identifier Expr
          | Alloc Expr
          | Dref Expr
          | Assign Expr Expr
          | Seq Expr Expr
          deriving (Eq)

instance Show Expr where
    show e = case e of
        Var x -> show x
        I x -> show x
        B x -> show x
        Add x y -> show x ++ " + " ++ show y
        Mul x y -> show x ++ " * " ++ show y
        Succ x -> show x
        Pred x -> show x
        And x y -> show x ++ " ∧ " ++show y
        Or x y -> show x ++ " ∨ " ++show y
        Not x -> "¬ " ++ show x
        Iszero x -> show x
        Lt x y -> show x ++ ">" ++ show y
        Gt x y -> show x ++ "<" ++ show y
        Eq x y -> show x ++ " = " ++ show y
        If x y z -> " if " ++ show x ++ " them " ++ show y ++ " else " ++ show z
        Let x y z -> "Let " ++ show x ++ " = " ++ show y ++ show z
        Fn x y -> show x ++ show y
        App x y -> show x ++ show y
        L x -> show x
        Alloc x -> show x
        Dref x -> show x
        Assign x y -> show x ++ " = " ++ show y
        Void -> "Void"
        Seq x y -> show x ++ show y
        While x y -> show x ++ show y
        Abs x y -> show x ++ show y
        Raise x -> "raised " ++ show e
        (Handle x y z) -> "handle " ++ show x ++ " with " ++ show y ++ " => " ++ show z

type Substitution = (Identifier, Expr)
