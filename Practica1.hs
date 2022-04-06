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
eval1 (Var v) = error "Variable libre"
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
-- eval1 Let (Num n) t = sustitucion ?
eval1 (Let t1 t2) = (Let (eval1(t1)) t2)
-- TODO faltan para Abs

evals :: EAB -> EAB
evals (Var v) = error "Variable libre"
evals (Num n) = (Num n)
evals (B b) = (B b)
evals (Sum e1 e2) = eval1(Sum (evals(e1)) (evals(e2)))
evals (Prod e1 e2) = eval1(Prod (evals(e1)) (evals(e2)))
-- TODO complete

eval :: EAB -> EAB
eval _ = error "Implementar"

-- ****************** Semantica Estatica ***********************
data Type = TBool 
          | TNat  deriving (Eq,Show)        -- Definir los tipos de EAB


type Ctx = [(Variable, Type)] -- Definir un sinomo para los contextos


tnum :: EAB -> Type -> Bool 
tnum (Num a) (TNat) = True 
tnum a b = False

tbool :: EAB -> Type -> Bool 
tbool (B a) (TBool) = True 
tbool a b = False

tnot :: EAB -> Type -> Bool 
tnot (Not a) (TBool) | tbool a TBool = True 
                     | otherwise = False 
tnot a b = False 

ttrue :: EAB -> Type -> Bool 
ttrue (B True) (TBool) = True 
ttrue a b = False 

tfalse :: EAB -> Type -> Bool 
tfalse (B False) TBool = True 
tfalse a b = False

tsum :: EAB -> Type -> Bool 
tsum (Sum a b) (TNat) | tnum a TNat && tnum b TNat = True 
                      | otherwise = False 
tsum a b = False

tprod :: EAB -> Type -> Bool 
tprod (Prod a b) (TNat) | tnum a TNat && tnum b TNat = True 
                        | otherwise = False 
tprod a b = False

tsuc :: EAB -> Type -> Bool 
tsuc (Suc a) (TNat) | tnum a (TNat) = True 
                    | otherwise = False 
tsuc a b = False


tpred :: EAB -> Type -> Bool 
tpred (Pred a) (TNat) | tnum a (TNat) = True 
                    | otherwise = False 
tpred a b = False

tand :: EAB -> Type -> Bool 
tand (And a b) (TBool) | (ttrue a (TBool)) && (ttrue b TBool) = True 
                       | otherwise  = False 
tand a b = False 

tor :: EAB -> Type -> Bool 
tor (Or a b) (TBool) | (ttrue a TBool) && (ttrue b TBool) = True
                     | ttrue a TBool && tfalse b TBool = True 
                     | tfalse a TBool && ttrue b TBool = True 
                     | otherwise  = False 
tor a b = False 

tisz :: EAB -> Type -> Bool 
tisz (Iszero a) (TNat) | tnum (Num 0) (TNat) = True 
                       | otherwise = False 

tisz a b = False 



tlet :: EAB -> Type -> Bool
tlet (Let a b) c | tnum a c && tnum b c = True 
                 | tprod a c && tprod b c = True 
                 | tnum a c && tprod b c = True 
                 | tprod a c && tprod b c =True 
                 | tsum a c && tsum b c = True 
--                 | tand a c && tand a c = True 
--                 | tor a c && tor a c = True 
--                 | tor a c && tand a c = True 
--                 | tand a c && tor a c = True 
--                 | tisz b c = True 
                 | otherwise = False

tif :: EAB -> Type -> Bool 
tif (If a b c) d | tbool a (TBool) && tnum a d && tnum c d = True 
                 | tbool a TBool && tbool a d && tbool c d = True 
                 | tbool a (TBool) && tsum b d && tnum c d = True
                 | tbool a (TBool) && tprod b d && tnum c d = True 
                 | tbool a (TBool) && tand b d && tbool c d = True 
                 | tbool a (TBool) && tor b d && tbool c d = True 
                 | tbool a TBool && tisz b d && tisz c d = True 
                 |otherwise = False 
                 


vt :: Ctx -> EAB -> Type -> Bool
vt [] (Num e) t = tnum (Num e) t
vt [] (B e) t = tbool (B e) t
vt [] (Sum e1 e2) t = tsum (Sum e1 e2) t
vt [] (Prod e1 e2) t = tprod (Prod e1 e2) t
vt ((a,b):xs) (Sum (Var e1) e2) t | b == t && a == e1 = tnum e2 t
vt ((a,b):xs) (Sum e1 (Var e2)) t | b == t && a == e2 = tnum e1 t
vt ((a,b):xs) (Prod (Var e1) e2) t | b == t && a == e1 = tnum e2 t
vt ((a,b):xs) (Prod e1 (Var e2)) t | b == t && a == e2 = tnum e1 t
vt ((a,b):xs) (And (Var e1) e2) t | b == t && a == e1 = tnum e2 t
vt ((a,b):xs) (And e1 (Var e2)) t | b == t && a == e2 = tnum e1 t
vt ((a,b):xs) (Or (Var e1) e2) t | b == t && a == e1 = tnum e2 t
vt ((a,b):xs) (Or e1 (Var e2)) t | b == t && a == e2 = tnum e1 t
vt a e t = False














evalt :: EAB -> EAB
evalt (Var a) = error "Variable libre"
evalt (Num a) = Num a 
evalt (B a) = B a

evalt (Sum (Num a) (Num b)) = Num (a+b)
evalt (Sum (Num a) (Var b)) = error "Existe una variable libre"
evalt (Sum (Var a) (Num b)) = error "Existencia de variable libre"
evalt (Sum (Num a)(B b)) = error "Error de tipo"
evalt (Sum (B a)(Num b)) = error "Error de tipo"
evalt (Sum (B a) (B b)) = error "No se puede realizar sumar"
evalt (Sum a b) = error "Error de tipado"

evalt (Prod (Num a) (Num b)) = Num (a*b)
evalt (Prod (Num a) (Var b)) = error "Existe una variable libre"
evalt (Prod (Var a) (Num b)) = error "Existencia de variable libre"
evalt (Prod (Num a)(B b)) = error "Error de tipo"
evalt (Prod (B a)(Num b)) = error "Error de tipo" 
evalt (Prod (B a) (B b)) = error "No se puede realizar sumar"
evalt (Prod a b) = error "Error de tipado"

evalt (Neg (B True)) = Neg (B False)
evalt (Neg (B False)) = Neg (B True)
evalt (Neg (Var a)) = error "Variable libre"
evalt (Neg (Num a)) = error "Error de tipo"
evalt (Neg a) = error "Error de tipado"

evalt (Pred (Num a)) | a < 1 = error "No se puede realizar"
                     | otherwise = Num (a-1)
evalt (Pred (Var a)) = error "Error de tipo"
evalt (Pred (B a)) = error "Error de tipo"


evalt (Suc (Num a)) |  a >= 0  = Num(a +1)
                     | otherwise = error "No se puede realizar"
evalt (Suc (Var a)) = error "Error de tipo"
evalt (Suc (B a)) = error "Error de tipo"

evalt (And (B True)(B True)) = B True 
evalt (And (B False)(B True)) = B False  
evalt (And (B True)(B False)) = B False 
evalt (And (B False)(B False)) = B False  
evalt (And a b) = error "Error de tipado"

evalt (Or (B True)(B True)) = B True 
evalt (Or (B False)(B True)) = B True   
evalt (Or (B True)(B False)) = B True  
evalt (Or (B False)(B False)) = B False  
evalt (Or a b) = error "Error de tipado"

evalt (Iszero (Num a)) | a == 0 = B True  
                       | a /= 0 = B False
                       | otherwise = error "Error de tipado"

