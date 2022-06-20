module Practica4 where

import Sintax
import Data.Bool

-- Alias para direcciones de memoria.
type Address = Int
{-- Alias para valores. Aunque por implementacion se podria poner cualquier expresion, se espera solo
sean valores. --}

type Value = Expr

type Cell = ( Address , Value )

data State = E Stack Expr
           | R Stack Expr
           | P Stack Expr

instance Show State where
    show e = case e of
        E x y -> show x ++ " > " ++ show y
        R x y -> show x ++ " < " ++ show y
        P x y -> show x ++ " << " ++ show y

-- Definicion de marcos
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
            | AllocF
            | DrefF
            | AssignFL Expr | AssignFR Expr
            | SeqF Expr
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
        (LetF e t) -> "Let(" ++ show e ++ ", _, " ++ show t ++ ")"
        AppFL x -> "App( - , " ++ show x ++ " )"
        AppFR x -> "App( " ++ show x ++ " , - )"
        AllocF  -> "Alloc( - )"
        DrefF  -> "Dref( - )"
        AssignFL x -> "Assing( - , " ++ show x ++" )"
        AssignFR x -> "Assing( " ++ show x ++" , - )"
        SeqF x -> "Seq("++ show x ++ " )"


--Definicion de pila de marcos
data Stack = Empty
           | S Frame Stack

instance Show Stack where
    show e = case e of
        Empty -> "[ ]"
        S x y -> show x ++ show y

{--
eval1. Re implementa esta función para que dado un estado, devuelva un paso de transicion,
esdecir,eval1s=s’siysólosis→k s’
--}
--- VALUES
eval1 (E s e@(I n)) = R s e
eval1 (E s e@(B b)) = R s e
eval1 (E s e@(Fn x exp)) = R s e

--- ADD
eval1 (E s (Add e1 e2)) = E (S (AddFL e2) s) e1
eval1 (R (S (AddFL e2) s) v) = E (S (AddFR v) s) e2
eval1 (R (S (AddFR (I v1)) s) (I v2)) = R s (I (v1+v2))

--- MUL
eval1 (E s (Mul e1 e2)) = E (S (MulFL e2) s) e1
eval1 (R (S (MulFL e2) s) v) = E (S (MulFR v) s) e2
eval1 (R (S (MulFR (I v1)) s) (I v2)) = R s (I (v1*v2))

--- SUCC
eval1 (E s (Succ e)) = E (S SuccF s) e
eval1 (R (S SuccF s) (I v)) = R s (I (v+1))

--PRED
eval1 (E s (Pred e)) = E (S PredF s) e
eval1 (R (S PredF s) (I v)) = R s (I (v-1))

--AND
eval1 (E s (And e1 e2)) = E (S (AndFL e2) s) e1
eval1 (R (S (AndFL e2) s) v) = E (S (AndFR v) s) e2
eval1 (R (S (AndFR (B v1)) s) (B v2)) = R s (B (v1 && v2))

--OR
eval1 (E s (Or e1 e2)) = E (S (OrFL e2) s) e1
eval1 (R (S (OrFL e2) s) v) = E (S (OrFR v) s) e2
eval1 (R (S (OrFR (B v1)) s) (B v2)) = R s (B (v1 || v2))

--- NEG
eval1 (E s (Not e)) = E (S NotF s) e
eval1 (R (S NotF s) (B v)) = R s (B (not v))

--- ISZERO
eval1 (E s (Iszero e)) = E (S IszeroF s) e
eval1 (R (S IszeroF s) (I v)) = R s (B (0 == v))

--LT
eval1 (E s (Lt e1 e2)) = E (S (LtFL e2) s) e1
eval1 (R (S (LtFL e2) s) v) = E (S (LtFR v) s) e2
eval1 (R (S (LtFR (I v1)) s) (I v2)) = R s (B (v1 < v2))

--GT
eval1 (E s (Gt e1 e2)) = E (S (GtFL e2) s) e1
eval1 (R (S (GtFL e2) s) v) = E (S (GtFR v) s) e2
eval1 (R (S (GtFR (I v1)) s) (I v2)) = R s (B (v1 > v2))

--EQ
eval1 (E s (Eq e1 e2)) = E (S (EqFL e2) s) e1
eval1 (R (S (EqFL e2) s) v) = E (S (EqFR v) s) e2
eval1 (R (S (EqFR (I v1)) s) (I v2)) = R s (B (v1 == v2))

--APP
eval1 (E s (App e1 e2)) = E (S (AddFL e2) s) e1
eval1 (R (S (AppFL e2) s) v) =  E (S (AppFR v) s) e2
eval1 (R (S (AddFR (Fn x y)) s) v) = case v of
                                       (I n) -> R s (subst y (x, v))
                                       (B b) -> R s (subst y (x, v))
                                       (Fn x e) -> R s (subst y (x, v))

-- IF
eval1 (E s (If e1 e2 e3)) = E (S (IfF e2 e3) s) e1
eval1 (R (S (IfF e2 e3) s) v) = case v of
                                  (B True) -> E s e2
                                  (B False) -> E s e3

--
frVars :: Expr -> [Identifier]
frVars (Var x) = [x]
frVars (I x) = []
frVars (B x)  = []
frVars (L x) = []
frVars (Add x y) = frVars x ++ frVars y
frVars (Mul x y) = frVars x ++ frVars y
frVars (Succ x) = frVars x
frVars (Pred x) = frVars x
frVars (And x y) = frVars x ++ frVars y
frVars (Or x y) = frVars x ++ frVars y
frVars (Not x) = frVars x
frVars (Iszero x) = frVars x
frVars (Lt x y) = (frVars x) ++ (frVars y)
frVars (Gt x y) = (frVars x)++ (frVars y)
frVars (Eq x y) = (frVars x) ++ (frVars y)
frVars (If x y z) = frVars x ++ frVars y ++ frVars z
frVars (Let x y z) = frVars y ++ frVars z
frVars (Fn x y) = (frVars y)
frVars (App x y) = frVars x ++ frVars y

--
subst :: Expr -> Substitution -> Expr
subst (Var x) (a, b) = if x == a
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

