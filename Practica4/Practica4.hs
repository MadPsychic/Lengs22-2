module Practica4 where 

import Sintax
import Data.Bool

-- Alias para direcciones de memoria.
type Address = Int
{-- Alias para valores. Aunque por implementacion se podria poner cualquier expresion, se espera solo
sean valores. --}

type Value = Expr

type Cell = ( Address , Value )

type Memory = [ Cell ]

data State = E Stack Memory Expr
           | R Stack Memory Expr
           | P Stack Memory Expr

instance Show State where
    show e = case e of 
        E x y z -> show x ++ show y ++ show z
        R x y z -> show x ++ show y ++ show z
        P x y z -> show x ++ show y ++ show z



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
            | LetM Identifier Expr
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
     -- TODO LetF id t -> ?
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

eval1 :: State -> State
--- VALUES
eval1 (E s m e@(I n)) = R s m e
eval1 (E s m e@(B b)) = R s m e
eval1 (E s m e@(Fn x exp)) = R s m e

--- ADD
eval1 (E s m (Add e1 e2)) = E (S (AddFL e2) s) m e1
eval1 (R (S (AddFL e2) s) m v) = E (S (AddFR v) s) m e2
eval1 (R (S (AddFR (I v1)) s) m (I v2)) = R s m (I (v1+v2))

--- MUL
eval1 (E s m (Mul e1 e2)) = E (S (MulFL e2) s) m e1
eval1 (R (S (MulFL e2) s) m v) = E (S (MulFR v) s) m e2
eval1 (R (S (MulFR (I v1)) s) m (I v2)) = R s m (I (v1+v2))
--- SUCC
eval1 (E s m (Succ e)) = E (S SuccF s) m e
eval1 (R (S SuccF s) m (I v)) = R s m (I (v+1))
--PRED
eval1 (E s m (Pred e))= E (S PredF s) m e
eval1 (R (S PredF s) m (I v)) = R s m (I (1-v))
--AND
eval1 (E s m (And e1 e2)) = E (S (AndFL e2) s) m e1
eval1 (R (S (AndFL e2) s) m v) = E (S (AndFR v) s) m e2
eval1 (R (S (AndFR (B v1)) s) m (B v2)) = R s m (B (v1 && v2))
--OR
eval1 (E s m (Or e1 e2)) = E (S (OrFL e2) s) m e1
eval1 (R (S (OrFL e2) s) m v) = E (S (OrFR v) s) m e2
eval1 (R (S (OrFR (B v1)) s) m (B v2)) = R s m (B (v1 || v2))
--- NEG
eval1 (E s m (Not e)) = E (S NotF s) m e
eval1 (R (S NotF s) m (B v)) = R s m (B (not v))
--- ISZERO
eval1 (E s m (Iszero e)) = E (S IszeroF s) m e
eval1 (R (S IszeroF s) m (I v)) = R s m (B (0 == v))
--LT
eval1 (E s m (Lt e1 e2)) = E (S (LtFL e2) s) m e1
eval1 (R (S (LtFL e2) s) m v) = E (S (LtFR v) s) m e2
eval1 (R (S (LtFR (Var v1)) s) m (Var v2)) = R s m (Var (v2))
--GT
eval1 (E s m (Gt e1 e2)) = E (S (GtFL e2) s) m e1
eval1 (R (S (GtFL e2) s) m v) = E (S (GtFR v) s) m e2
eval1 (R (S (GtFR (Var v1)) s) m (Var v2)) = R s m (Var (v1))
--EQ
eval1 (E s m (Eq e1 e2)) = E (S (EqFL e2) s) m e1
eval1 (R (S (EqFL e2) s) m v) = E (S (EqFR v) s) m e2
eval1 (R (S (EqFR (Var v1)) s) m (Var v2)) = R s m (B (v1 == v2))




--- WHILE
eval1 (E s m w@(While f e)) = E s m (If f (Seq e w) Void)


