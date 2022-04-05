module EAB where
type Variable = String

data EAB = Var Variable
         | Num Int
         | B Bool
         | Sum EAB EAB
         | Prod EAB EAB
         | Neg EAB
         | Pred EAB
         | Suc EAB
         | And EAB EAB
         | Or EAB EAB
         | Not EAB
         | Iszero EAB
         | Let EAB EAB
         | If EAB EAB EAB
         | Abs Variable EAB deriving (Show, Eq)

eval1 :: EAB -> EAB
eval1 Var v = error "Variable libre"
eval1 Num n = Num n
eval1 B b = B b
eval1 Sum (Num n) (Num m) = (Num (n+m))
eval1 Sum t1 t2 = (Sum eval1(t1) t2)
eval1 Sum (Num n) t = (Sum (Num n) eval1(t))
eval1 Prod (Num n) (Num m) = (Num (n*m))
eval1 Prod t1 t2 = (Prod eval1(t1) t2)
eval1 Prod (Num n) t = (Prod (Num n) eval1(t))
eval1 Neg (Num n) = (Num (-n))
eval1 Neg t = (Neg eval1(t))
eval1 Suc (Num n) = (Num (n+1))
eval1 Suc t = (Suc eval1(t))
eval1 Pred (Num n) = (Num (n-1))
eval1 Pred t = (Pred eval1(t))
eval1 Not (B True) = (B False)
eval1 Not (B False) = (B True)
eval1 Not t = (Not eval1(t))
eval1 And (B True) (B True) = (B True)
eval1 And (B a) (B b) = (B False)
eval1 And t1 t2 = (And eval1(t1) t2)
eval1 And (B b) t = (And (B b) eval1(t))
eval1 Or (B False) (B False) = (B False)
eval1 Or (B a) (B b) = (B True)
eval1 Or t1 t2 = (Or eval1(t1) t2)
eval1 Or (B b) t = (Or (B b) eval1(t))
eval1 If (B True) t2 t3 = t2
eval1 If (B False) t2 t3 = t3
eval1 If t1 t2 t3 = (If eval1(t1) t2 t3)
eval1 Iszero (Num 0) = (B True)
eval1 Iszero (Num n) = (B False)
eval1 Iszero t = (Iszero eval1(t))
-- eval1 Let (Num n) t = sustitucion ?
eval1 Let t1 t2 = (Let eval1(t1) t2)
-- TODO faltan para Abs

evals :: EAB -> EAB
evals Var v = error "Variable libre"
evals Num n = Num n
evals B b = B b
evals Sum e1 e2 = eval1(Sum evals(e1) evals(e2))
evals Prod e1 e2 = eval1(Prod evals(e1) evals(e2))
-- TODO complete

eval :: EAB -> EAB
eval _ = error "Implementar"

data Type = () -- Definir los tipos de EAB
type Ctx = () -- Definir un sinomo para los contextos

vt :: Ctx -> EAB -> Type -> Bool
vt _ _ _ = error "Implementar"

evalt :: EAB -> EAB
evalt _ = error "Implementar"           
