module Practica3 where

import Sintax
import Data.List
import Data.Maybe

type Address = Int

type Value = Expr
type Cell = (Address, Value)
type Memory = [Cell]

data Frame = AddFL Expr | AddFR Expr
            | MulFL Expr | MulFR Expr
            | SuccF | PredF
            | AndFL Expr | AndFR Expr
            | OrFL Expr | OrFR Expr
            | NotF | IszeroF
            | LtFL Expr | LtFR Expr
            | GtFL Expr | GtFR Expr
            | EqFL Expr | EqFR Expr
            | IfF Expr Expr
            | LetF Identifier Expr
            | AppFL Expr | AppFR Expr
            deriving (Eq)

instance Show Frame where
    show e = case e of
        AddFL x -> "Add( - , " ++ show x ++ " )"
        AddFR x -> "Add( " ++ show x ++ " , - )"
        MulFL x -> "Mul( - , " ++ show x ++ " )"
        MulFR x -> "Mul( " ++ show x ++ " , - )"
        SuccF -> "Succ( - )"
        PredF -> "Pred( - )"
        AndFL x -> "And( - , " ++ show x ++ " )"
        AndFR x -> "And( " ++ show x ++ " , - )"
        OrFL x -> "Or( - , " ++ show x ++ " )"
        OrFR x -> "Or( " ++ show x ++ " , - )"
        NotF -> "Not( - )"
        IszeroF -> "IsZero( - )"
        LtFL x -> "Lt( - , " ++ show x ++ " )"
        LtFR x -> "Lt( " ++ show x ++ " , - )"
        GtFL x -> "Gt( - , " ++ show x ++ " )"
        GtFR x -> "Gt( " ++ show x ++ " , - )"
        EqFL x -> "Eq( - , " ++ show x ++ " )"
        EqFR x -> "Eq( " ++ show x ++ " , - )"
        (IfF e t) -> "If( - , " ++ show e ++ show t ++ " )"
     -- TODO LetF id t -> ?
        AppFL x -> "App( - , " ++ show x ++ " )"
        AppFR x -> "App( " ++ show x ++ " , - )"

data Stack = Empty | S Frame Stack

data State = E Stack Memory Expr
           | R Stack Memory Expr

eval1 :: State -> State
eval1 = error "implementar"

-- *****************   Test eval1 ***********************
-- eval1T0 = eval1 (E Empty (Add (I 2) (I 3)) )
-- eval1T1 = eval1 E (S (AddFL (I 3)) Empty) (I 2)

evals :: State -> State
evals = error "implementar"

-- *****************   Test eval1 ***********************
-- evals0 = evals (E Empty (Let "x" ( I 2 ) (Mul (Add ( I 1 ) ( V "x" ) ) (V "x" ) ) ) )
-- evals1 = evals (E Empty (Let "x" (B True) (If (Var "x") (Var "x") (B False) ) ) )

evale :: Expr -> Expr
evale = error "implementar"

-- *****************   Test eval1 ***********************
evale0 = evale (Add (Mul ( I 2 ) ( I 6 ) ) (B True) )
evale1 = evale (Or (Eq (Add ( I 0 ) ( I 0 ) ) ( I 0 ) ) (Eq ( I 1 ) (I 10) ) )

-- ***************** Manejo de excepciones ***********************
{-- TODO frVars. Extiende esta función para las nuevas expresiones.
frVars :: Expr -> [ Identifier ]
frVars (Var v) = [v]
frVars (I x) = []
frVars (B x)  = []
frVars (L x ) = []
frVars (Add x y) = (frVars x) ++ (frVars y)
frVars (Mul x y) = (frVars x)++ (frVars y)
frVars (Succ x) = (frVars x)
frVars (Pred x) = (frVars x)
frVars (And x y) = (frVars x)++ (frVars y)
frVars (Or x y) = (frVars x) ++ (frVars y)
frVars (Not x) = (frVars x)
frVars (Iszero x) = (frVars x)
frVars (Lt x y) = (frVars x) ++ (frVars y)
frVars (Gt x y) = (frVars x)++ (frVars y)
frVars (Eq x y) = (frVars x) ++ (frVars y)
frVars (If x y z)= (frVars x) ++ (frVars y) ++ (frVars z)
frVars (Let x y z) = (frVars y) ++ (frVars z)
frVars (Fn x y) = (frVars y)
frVars (App x y) = (frVars x) ++ (frVars y)
frVars (Alloc x) = (frVars x)
frVars (Dref x) = (frVars x)
frVars (Assign x y) = (frVars x) ++ (frVars y)
frVars Void = []
frVars (Seq x y)= (frVars x) ++ (frVars y)
frVars (While x y) = (frVars x) ++ (frVars y)
frVars (Abs x a1) = filter (/= x) (frVars a1)


-- TODO subst. Extiende esta función para las nuevas expresiones.
subst :: Expr -> Substitution -> Expr
subst (Var x) (a,b) = if x == a
                      then b
                      else Var x
subst (I x) _ = I x
subst (B x) _ = B x
subst (L x ) _ = L x
subst (Add x y) e =  Add (subst x e) (subst y e)
subst (Mul x y) e = Mul (subst x e) (subst y e)
subst (Succ x) e = Succ (subst x e)
subst (Pred x) e = Pred (subst x e)
subst (And x y) e = And (subst x e) (subst y e)
subst (Or x y) e = Or(subst x e) (subst y e)
subst (Not x) e = Not(subst x e)
subst (Iszero x) e = Iszero(subst x e)
subst (Lt x y) e = Lt (subst x e) (subst y e)
subst (Gt x y) e = Gt (subst x e) (subst y e)
subst (Eq x y) e = Eq (subst x e) (subst y e)
subst (If x y z) e = If (subst x e) (subst y e) (subst z e)
subst (Let x y z) e = Let x (subst y e) (subst z e)
subst (Fn x y) e = Fn x (subst y e)
subst (App x y) e = App (subst x e) (subst y e)
subst (Alloc x) e = Alloc (subst x e)
subst (Dref x) e = Dref (subst x e)
subst (Assign x y) e = Assign (subst x e) (subst y e)
subst Void e = Void
subst (Seq x y) e= Seq (subst x e) (subst y e)
subst (While x y) e = While (subst x e) (subst y e)
subst (Abs z e) s@(x,r)
  | z == x || elem z (frVars r) = error "Se requiere una equivalencia"
  | otherwise = Abs z (subst e s)
-}

{-- TODO Extiende las funciones de eval1, evals y evale para incluir el manejo de excepciones con valor
usando las expresiones que agregamos a la sintaxis.

TEST
eval1 P (S (HandleF "x" (V "x")) Empty) (Raise (B False))
-}
