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

type Subst = (Variable,EAB)

fv :: EAB -> [Variable]
fv (Var v) = [v]
fv (Num _) = []
fv (B _) = []
fv (Sum a1 a2) = fv a1 ++ fv a2
fv (Prod a1 a2) = fv a1 ++ fv a2
fv (Neg e) = fv e
fv (Pred e) = fv e
fv (Suc e) = fv e
fv (And e t) = fv e ++ fv t
fv (Or e t) = fv e ++ fv t
fv (Not e) = fv e
fv (Iszero e) = fv e
fv (If t1 t2 t3) = fv t1 ++ fv t2 ++ fv t3
fv (Let a1 a2) = fv a1 ++ fv a2
fv (Abs x a1) = filter (/= x) (fv a1)

subs :: EAB -> Subst -> EAB
subs (Var v) (x,e) = if v == x
                     then e
                     else (Var v)
subs (Num n) _ = Num n
subs (B b) _ = B b
subs (Sum a1 a2) s = Sum (subs a1 s) (subs a2 s)
subs (Prod a1 a2) s = Prod (subs a1 s) (subs a2 s)
subs (Neg e) s = Neg (subs e s)
subs (Pred e) s = Pred (subs e s)
subs (Suc e) s = Suc (subs e s)
subs (And e t) s = And (subs e s) (subs t s)
subs (Or e t) s = Or (subs e s) (subs t s)
subs (Not e) s = Not (subs e s)
subs (Iszero e) s = Iszero (subs e s)
subs (If t1 t2 t3) s = If (subs t1 s) (subs t2 s) (subs t3 s)
subs (Let a1 a2) s = Let (subs a1 s) (subs a2 s)
subs (Abs z e) s@(x,r)
  | z == x || elem z (fv r) = error "Se requiere una equivalencia"
  | otherwise = Abs z (subs e s)

eval1 :: EAB -> EAB
eval1 (Var v) = (Var v)
eval1 (Num n) = (Num n)
eval1 (B b) = (B b)
eval1 (Sum (Num n) (Num m)) = (Num (n+m))
eval1 (Sum (Num n) t) = (Sum (Num n) (eval1(t)))
eval1 (Sum t1 t2) = (Sum (eval1(t1)) t2)
eval1 (Prod (Num n) (Num m)) = (Num (n*m))
eval1 (Prod (Num n) t) = (Prod (Num n) (eval1(t)))
eval1 (Prod t1 t2) = (Prod (eval1(t1)) t2)
eval1 (Neg (Num n)) = (Num (-n))
eval1 (Neg t) = (Neg (eval1(t)))
eval1 (Suc (Num n)) = (Num (n+1))
eval1 (Suc t) = (Suc (eval1(t)))
eval1 (Pred (Num n)) = (Num (n-1))
eval1 (Pred t) = (Pred (eval1(t)))
eval1 (Not (B True)) = (B False)
eval1 (Not (B False)) = (B True)
eval1 (Not t) = (Not (eval1(t)))
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
eval1 (Iszero (Num 0)) = (B True)
eval1 (Iszero (Num n)) = (B False)
eval1 (Iszero t) = (Iszero (eval1(t)))
eval1 (Let (B b) (Abs x e)) = (subs e (x,(B b)))
eval1 (Let (Num n) (Abs x e)) = (subs e (x,(Num n)))
eval1 (Let t1 t2) = (Let (eval1(t1)) t2)

evals :: EAB -> EAB
evals (Var v) = (Var v)
evals (Num n) = (Num n)
evals (B b) = (B b)
evals (Sum e1 e2) = eval1(Sum (evals(e1)) (evals(e2)))
evals (Prod e1 e2) = eval1(Prod (evals(e1)) (evals(e2)))
evals (Neg e) = eval1(Neg (evals(e)))
evals (Suc e) = eval1(Suc (evals(e)))
evals (Pred e) = eval1(Pred (evals(e)))
evals (Not e) = eval1(Not (evals(e)))
evals (And e t) = eval1(And (evals(e)) (evals(t)))
evals (Or e t) = eval1(Or (evals(e)) (evals(t)))
evals (If t1 t2 t3) = eval1(If (evals(t1)) (evals(t2)) (evals(t3)))
evals (Iszero e) = eval1(Iszero (evals(e)))
evals (Let e t) = eval1(Let (evals(e)) (evals(t)))

eval :: EAB -> EAB
eval (Var v) = error "Variable libre"
eval (Num n) = (Num n)
eval (B b) = (B b)
eval (Sum e1 e2) = eval1(Sum (eval(e1)) (eval(e2)))
eval (Prod e1 e2) = eval1(Prod (eval(e1)) (eval(e2)))
eval (Neg e) = eval1(Neg (eval(e)))
eval (Suc e) = eval1(Suc (eval(e)))
eval (Pred e) = eval1(Pred (eval(e)))
eval (Not e) = eval1(Not (eval(e)))
eval (And e t) = eval1(And (eval(e)) (eval(t)))
eval (Or e t) = eval1(Or (eval(e)) (eval(t)))
eval (If t1 t2 t3) = eval1(If (eval(t1)) (eval(t2)) (eval(t3)))
eval (Iszero e) = eval1(Iszero (eval(e)))
eval (Let e t) = eval1(Let (evals(e)) (evals(t)))
-- TODO debe devolver error para expresiones que no se pueden interpretar

data Type = () -- Definir los tipos de EAB
type Ctx = () -- Definir un sinomo para los contextos

vt :: Ctx -> EAB -> Type -> Bool
vt _ _ _ = error "Implementar"

evalt :: EAB -> EAB
evalt _ = error "Implementar"           
