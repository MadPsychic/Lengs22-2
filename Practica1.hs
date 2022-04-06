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
evals (Abs x e) = (Abs x e)

isNum :: EAB -> Bool
isNum (Num n) = True
isNum _ = False

hasBool :: EAB -> Bool
hasBool (B _) = True
hasBool (Var v) = False
hasBool (Num n) = False
hasBool (Iszero (Num n)) = True
hasBool (Iszero e) = isNum(evals e)
hasBool (Sum e t) = hasBool(evals e) || hasBool(evals t)
hasBool (Prod e t) = hasBool(evals e) || hasBool(evals t)
hasBool (Neg e) = hasBool(evals e)
hasBool  (Suc e) = hasBool(evals e)
hasBool (Pred e) = hasBool(evals e)
hasBool (Not e) = hasBool(evals e)
hasBool (And e t) = hasBool(evals e) || hasBool(evals t)
hasBool (Or e t) = hasBool(evals e) || hasBool(evals t)
hasBool (If t1 t2 t3) =  hasBool(evals(t1)) || hasBool(evals(t2)) || hasBool(evals(t3))
hasBool (Let e t) = hasBool(evals e) || hasBool(evals t)
hasBool (Abs x e) = hasBool(evals e)

eval :: EAB -> EAB
eval (Var v) = error "Variable libre"
eval (Num n) = (Num n)
eval (B b) = (B b)
eval (Sum e t) = if (fv (Sum e t) /= [] || hasBool(e) || hasBool(t))
                 then error "Variable libre o bool en suma"
                 else (evals (Sum e t))
eval (Prod e t) = if (fv (Prod e t) /= [] || hasBool(e) || hasBool(t))
                  then error "Variable libre o bool en producto"
                  else (evals (Prod e t))
eval (Neg e) = if (fv e /= [] || hasBool(e))
               then error "Variable libre o bool en negativo aritmético"
               else (evals (Neg e))
eval (Suc e) = if (fv e /= [] || hasBool(e))
               then error "Variable libre o bool en sucesor"
               else (evals (Suc e))
eval (Pred e) = if (fv e /= [] || hasBool(e))
               then error "Variable libre o bool en predecesor"
               else (evals (Pred e))
eval (Not e) = if (fv e /= [] || isNum(evals e))
               then error "Variable libre o número en negación"
               else (evals (Not e))
eval (And e t) = if (fv (And e t) /= [] || isNum(evals e) || isNum(evals t))
                 then error "Variable libre o número en conjunción"
                 else (evals (And e t))
eval (Or e t) = if (fv (Or e t) /= [] || isNum(evals e) || isNum(evals t))
                then error "Variable libre o número en disyunción"
                else (evals (Or e t))
eval (If t1 t2 t3) = if (fv (If t1 t2 t3) /= [] || isNum(evals t1))
                     then error "Variable libre en If o número en condición de If"
                     else (evals (If t1 t2 t3))
eval (Iszero e) = if (fv e /= [] || hasBool(evals e))
                  then error "Variable libre o bool en operación Iszero"
                  else (evals (Iszero e))
eval (Let e (Abs x t)) = if fv (Let e (Abs x t)) /= []
                         then error "Variable libre en Let"
                         else eval(evals (Let e (Abs x t)))
eval e = error "Expresión mal formada"

data Type = () -- Definir los tipos de EAB
type Ctx = () -- Definir un sinomo para los contextos

vt :: Ctx -> EAB -> Type -> Bool
vt _ _ _ = error "Implementar"

evalt :: EAB -> EAB
evalt _ = error "Implementar"           
