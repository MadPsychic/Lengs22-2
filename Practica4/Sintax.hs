module Sintax where

type Identifier = String

data Expr = Var Identifier | I Int | B Bool
          | Add Expr Expr | Mul Expr Expr
          | Succ Expr | Pred Expr
          | And Expr Expr | Or Expr Expr
          | Not Expr | Iszero Expr
          | Lt Expr Expr -- Mayor que 
          | Gt Expr Expr -- Menor que
          | Eq Expr Expr -- Igual
          | If Expr Expr Expr
          | Let Identifier Expr Expr
          | App Expr Expr
          | L Int
          | Void
          | While Expr Expr
-- ************** No las usaremos ***************

          | Abs Identifier Expr
          | Fn Identifier Expr
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





type Substitution = (Identifier, Expr)

{--
    Igual deben adecuar las funciones realizas en la practica 1 para ajustarse a esta sintaxis
--}


{-- ****************** Semántica Dinámica ***********************

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
-}
