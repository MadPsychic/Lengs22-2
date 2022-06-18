module Sintax where

type Identifier = String

data Expr = Var Identifier | I Int | B Bool
          | Add Expr Expr | Mul Expr Expr
          | Succ Expr | Pred Expr
          | And Expr Expr | Or Expr Expr
          | Not Expr | Iszero Expr
          | Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
          | If Expr Expr Expr
          | Let Identifier Expr Expr
          | Fn Identifier Expr
          | App Expr Expr
          | L Int
          | Alloc Expr
          | Dref Expr
          | Assign Expr Expr
          | Void
          | Seq Expr Expr
          | While Expr Expr
          | Abs Identifier Expr
          deriving (Show, Eq)



type Substitution = (Identifier, Expr)
{--
    Igual deben adecuar las funciones realizas en la practica 1 para ajustarse a esta sintaxis
--}


-- ****************** Semántica Dinámica ***********************

eval1 :: Expr -> Expr
eval1 (Var v) = (Var v)
eval1 (I n) = (I n)
eval1 (B b) = (B b)
eval1 (Add (I n) (I m)) = (I (n+m))
eval1 (Add (I n) t) = (Add (I n) (eval1(t)))
eval1 (Add t1 t2) = (Add (eval1(t1)) t2)
eval1 (Mul (I n) (I m)) = (I (n*m))
eval1 (Mul (I n) t) = (Mul (I n) (eval1(t)))
eval1 (Mul t1 t2) = (Mul (eval1(t1)) t2)
eval1 (Not (B True)) = (B False)
eval1 (Not (B False)) = (B True)
eval1 (Not t) = (Not (eval1 (t)))
eval1 (Succ (I n)) = (I (n+1))
eval1 (Succ t) = (Succ (eval1(t)))
eval1 (Pred (I n)) = (I (n-1))
eval1 (Pred t) = (Pred (eval1(t)))
eval1 (And (B True) (B True)) = (B True)
eval1 (And (B a) (B b)) = (B False)
eval1 (And (B b) t) = (And (B b) (eval1(t)))
eval1 (And t1 t2) = (And (eval1(t1)) t2)
eval1 (Or (B False) (B False)) = (B False)
eval1 (Or (B a) (B b)) = (B True)
eval1 (Or (B b) t) = (Or (B b) (eval1(t)))
eval1 (Or t1 t2) = (Or (eval1(t1)) t2)
eval1 (If (B True) t2 t3) = t2
eval1 (If (B False) t2 t3) = t3
eval1 (If t1 t2 t3) = (If (eval1(t1)) t2 t3)
eval1 (Iszero (I 0)) = (B True)
eval1 (Iszero (I n)) = (B False)
eval1 (Iszero t) = (Iszero (eval1(t)))
--eval1 (Let (B b) (Abs x y) (Abs e a)) = (Substitution e (x,(B b)))
--eval1 (Let (I n) (Abs x y) (Abs e a)) = (Substitution e (x,(I n)))
--eval1 (Let t1 t2 t3) = (Let (eval1(t1)) t2 t3)
