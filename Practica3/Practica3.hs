module Practica3 where

import Sintax
import Data.List
import Data.Maybe

type Address = Int

type Value = Expr
type Cell = (Address, Value)
type Memory = [Cell]


{--
 -- newAddress.Dada una memoria, genera una nueva direccion de memoria
 -- que no este contenida en esta.
--}

minFreeMemory :: [Address] -> Address
minFreeMemory xs = head ([0..]\\ xs)

repeatedMemory :: Eq a => [a] -> Bool
repeatedMemory[] = True
repeatedMemory [x] = True
repeatedMemory(x:xs) | x `elem` xs = False
                     | otherwise = repeatedMemory xs

listMemory :: Memory -> [Address]
listMemory[] = []
listMemory[x]=[fst x]
listMemory(x:xs)=[fst x] ++ listMemory xs

newAddress :: Memory -> Expr
newAddress xs | repeatedMemory(listMemory xs) = L (minFreeMemory (listMemory xs))
              | otherwise = error "Corrupted memory."


-- *****************   Test newAddress ***********************
newAddress1 = newAddress [ ]
newAddress2 = newAddress [ ( 0 , B False ) , ( 2 , I 9 ) ]
newAddress3 = newAddress [ ( 0 , I 21 ) , ( 1 , Void) , ( 2 , I 12 ) ]
newAddress4 = newAddress [ ( 0 , I 21 ) , ( 1 , Void) , ( 2 , I 12 ) , ( 1 , B True) ]

{--
 -- access.Dada una dreccion de memoria, devuelve el valor contenido en la celda con tal
 -- direccion, en caso de no encontrarla regresara Nothing
 --}

contains :: Eq a => a -> [a] -> Bool
contains = \elem -> \myList ->
  case myList of
    [] -> False
    x:xs | x == elem -> True
    _:xs -> contains elem xs

distintos :: Eq a => [a] -> Bool
distintos [] = False
distintos [x] = False
distintos (x:xs) | x `elem` xs = True
                 | otherwise = distintos xs

listValue :: Memory -> [Value]
listValue[] = []
listValue[x] = [snd x]
listValue(x:xs) = [snd x] ++ listValue xs


access ::  Address -> Memory -> Maybe Value
access n xs = if distintos (listMemory xs)
  then error "Corrupted memory"
  else if contains n (listMemory xs) then Just (Sintax.eval1 ((listValue xs) !!(n-1)))
       else Nothing

-- *****************   Test access ***********************
access1 = access 3 [ ]
access2 = access 1 [ ( 0 , B False ) , ( 2 , I 9 ) ]
access3 = access 2 [ ( 0 , I 21 ) , ( 2 , I 12 ) , ( 1 , Void) ]
access4 = access 2 [ ( 0 , I 21 ) , ( 0 , B False ) , ( 3 , Void) , ( 2 , I 12 ) ]

{--
 -- update. Dada una celda de memoria, actualiza el valor de esta misma en la memoria.
 -- En caso de no existir debe devolver Nothing.
 --}

memoryStoreValue :: Cell -> Bool
memoryStoreValue (a,b) =case b of
  Var x -> False
  I x -> False
  B x  -> False
  Fn x _ -> False
  _ -> True

-- replaceMemory :: Cell -> Memory -> Memory
-- replaceMemory y (x:xs) = if (fst y) == (fst x)
--  then y:replaceMemory y xs
--  else x:replaceMemory y xs

replaceMemory :: Cell -> Memory -> Memory
replaceMemory y (x:xs) = if fst y == fst x
                         then (fst x, snd y):xs
                         else [x] ++ replaceMemory y xs


update ::  Cell -> Memory -> Maybe Memory
update ys xs
  | distintos (listMemory xs) = error "Corrupted memory"
  | memoryStoreValue ys = error "Memory can only store values"
  | contains (fst ys) (listMemory xs) = Just (replaceMemory ys xs)
  | otherwise = Nothing

-- ******************* Test update +++++++++++++++++++++++
update1 = update ( 3 , B True) [ ]
update2 = update ( 0 , Succ (Var "x" ) ) [ ( 0 , B False ) , ( 2 , I 9 ) ]
update3 = update ( 0 , Fn "x" (Var "x" ) ) [ ( 0 , I 21 ) , ( 1 , Void ) , ( 2 , I 12 ) ]
update4 = update ( 2 , I 14 ) [ ( 0 , I 21 ) , ( 2 , Void ) , ( 2 , I 12 ) ]
update5 = update ( 2 , I 14 ) [ ( 0 , I 13 ) , ( 1 , B True ) , ( 2 , I 25 ) ]

{--
 -- frVars. Extiende esta función para las nuevas expresiones.
 --}

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

-- *++++++++++++++++++++ Test frVars ++++++++++++++++++++++++
frVars1 = frVars (Add (Var "x" ) ( I 5 ) )
frVars2 = frVars ( Assign (L 2 ) (Add ( I 0 ) (Var "z" ) ) )

{--
-- subst. Extiende esta función para las nuevas expresiones.
--}

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

-- ******************** Test subst ************************
subst1 = subst (Add (Var "x" ) ( I 5 ) ) ( "x" , I 10 )
subst2 = subst ( Let "x" ( I 1 ) (Var "x" ) ) ( "y" , Add (Var "x" ) ( I 5 ) )
subst3 = subst (Assign (L 2 ) (Add ( I 0 ) (Var "z" ) ) ) ("z" , B False )

eval1 :: (Memory, Expr) -> (Memory, Expr)
eval1 (mem, Var x) = (mem, Var x)
eval1 (mem, I x) = (mem, I x)
eval1 (mem, B x) = (mem, B x)
eval1 (mem, (Add (I n) (I m))) = (mem, I (n+m))
eval1 (mem, (Add e t)) = (mem, Add e t)
eval1 (mem, (Mul (I n) (I m))) = (mem, I (n*m))
eval1 (mem, (Mul e t)) = (mem, Mul e t)
eval1 (mem, Succ (I n)) = (mem, I (n+1))
eval1 (mem, Succ e) = error "Sólo hay sucesores para números"
eval1 (mem, Pred (I n)) = (mem, I (n-1))
eval1 (mem, Pred e) = error "Sólo hay predecesores para números"
eval1 (mem, Not (B b)) = (mem, B (not b))
eval1 (mem, Not e) = error "El not sólo funciona con booleanos"
eval1 (mem, (And (B b) (B o))) = (mem, B (b && o))
eval1 (mem, (And e t)) = (mem, And e t)
eval1 (mem, (Or (B b) (B o))) = (mem, B (b || o))
eval1 (mem, (Or e t)) = (mem, Or e t)
eval1 (mem, If (B False) e t) = (mem, t)
eval1 (mem, If (B True) e t) = (mem, e)
eval1 (mem, If c e t) = (memAux, (If nc e t))
  where (memAux, nc) = Practica3.eval1 (mem, c)
eval1 (mem, Iszero (I e)) = if e == 0
                            then (mem, B True)
                            else (mem, B False)
eval1 (mem, Iszero e) = error "No podemos determinar si un no-número es cero"
eval1 (mem, Let id (I n) (Fn x e)) = (mem, (subst e (x, (I n))))
eval1 (mem, Let id (B b) (Fn x e)) = (mem, (subst e (x, (B b))))
eval1 (mem, Let id t (Fn x e)) = (memAux, (Let id nt (Fn x e)))
  where (memAux, nt) = Practica3.eval1 (mem, t)
eval1 (mem, Lt (I n) (I m)) = (mem, B (n < m))
eval1 (mem, Lt e t) = (mem, Lt e t)
eval1 (mem, Gt (I n) (I m)) = (mem, B (n > m))
eval1 (mem, Gt e t) = (mem, Gt e t)
eval1 (mem, Eq (I n) (I m)) = (mem, B (n == m))
eval1 (mem, Eq e t) = (mem, Eq e t)
eval1 (mem, Seq Void e) = (mem, e)
eval1 (mem, Seq t e) = (memAux, Seq (s) (e))
  where (memAux, s) = Practica3.eval1 (mem, t)
eval1 (mem, While e t) = (mem, If e (Seq t (While e t)) Void)
eval1 (mem, Assign (L n) t) = case update (n, t) mem of
                                Nothing -> ([], Void)
                                Just (x:xs) -> ((x:xs), Void)

-- *****************   Test eval1 ***********************
evalT1 = Practica3.eval1 ( [ ( 0 , B False ) ] , (Add ( I 1 ) ( I 2 ) ) )
evalT2 = Practica3.eval1 ( [ ( 0 , B False ) ] , ( Let "x" ( I 1 ) (Fn "x" (Add (Var "x") (I 2 ) ) ) ) )
evalT3 = Practica3.eval1 ( [ ( 0 , B False ) ] , Assign (L 0 ) (B True) )
evalT4 = Practica3.eval1 ( [ ] , While (B True) (Add ( I 1 ) ( I 1 ) ) )

evals :: (Memory, Expr) -> (Memory, Expr)
evals (mem, Var x) = (mem, Var x)
evals (mem, I x) = (mem, I x)
evals (mem, B x) = (mem, B x)
evals (mem, e@(Add (I n) (I m))) = (mem, I (n+m))
evals (mem, e@(Add t p)) = Practica3.eval1 (mem3, f)
  where (mem1, f1) = Practica3.eval1 (mem, t)
        (mem2, f2) = Practica3.eval1 (mem1, p)
        (mem3, f) = Practica3.eval1 (mem2, Add f1 f2)
evals (mem, e@(Mul (I n) (I m))) = (mem, I (n*m))
evals (mem, e@(Mul t p)) = Practica3.eval1 (mem3, f)
  where (mem1, f1) = Practica3.eval1 (mem, t)
        (mem2, f2) = Practica3.eval1 (mem1, p)
        (mem3, f) = Practica3.eval1 (mem2, Mul f1 f2)
evals (mem, Succ (I n)) = (mem, I (n+1))
evals (mem, Succ e) = Practica3.eval1 (mem, Succ e)
evals (mem, Pred (I n)) = (mem, I (n-1))
evals (mem, Pred e) = Practica3.eval1 (mem, Pred e)
evals (mem, Not (B b)) = (mem, B (not b))
evals (mem, Not e) = Practica3.eval1 (mem, Not e)
evals (mem, e@(And (B b) (B o))) = (mem, B (b && o))
evals (mem, e@(And t p)) = Practica3.eval1 (mem3, f)
  where (mem1, f1) = Practica3.eval1 (mem, t)
        (mem2, f2) = Practica3.eval1 (mem1, p)
        (mem3, f) = Practica3.eval1 (mem2, And f1 f2)
evals (mem, e@(Or (B b) (B o))) = (mem, B (b || o))
evals (mem, e@(Or t p)) = Practica3.eval1 (mem3, f)
  where (mem1, f1) = Practica3.eval1 (mem, t)
        (mem2, f2) = Practica3.eval1 (mem1, p)
        (mem3, f) = Practica3.eval1 (mem2, Or f1 f2)
evals (mem, Iszero (I n)) = if n == 0
                            then (mem, B True)
                            else (mem, B False)
evals (mem, Iszero e) = Practica3.eval1 (memAux, f)
  where (memAux, f) = Practica3.eval1 (mem, e)
evals (mem, Let id e (Fn x t)) = Practica3.eval1 (memAux, f)
  where (mem1, f1) = Practica3.eval1 (mem, e)
        (memAux, f) = Practica3.eval1 (mem1, Let id f1 (Fn x t))
evals (mem, Eq e t) = Practica3.eval1 (memAux, f)
  where (mem1, f1) = Practica3.eval1 (mem, e)
        (mem2, f2) = Practica3.eval1 (mem, t)
        (memAux, f) = Practica3.eval1 (mem2, Eq f1 f2)
-- evals (mem, Assign e t) =

-- *****************   Test evals ***********************
evals1 = evals ( [ ] , ( Let "x" (Add ( I 1 ) ( I 2 ) ) (Fn "x" (Eq (Var "x" ) ( I 0 ) ) ) ) )
evals2 = evals ( [ ] , (Add (Mul ( I 2 ) ( I 6 ) ) (B True) ) )
evals3 = evals ( [ ] , Assign ( Alloc (B False ) ) ( Add ( I 1 ) ( I 9 ) ) )

evale :: Expr -> Expr
evale (Var x) = (Var x)
evale (I x) = (I x)
evale (B x) = (B x)
evale e@(Add t p) = evalInt e
evale e@(Mul t p) = evalInt e
evale e@(Succ t) = evalInt e
evale e@(Pred t) = evalInt e
evale e@(Not t) = evalB e
evale e@(And t p) = evalB e
evale e@(Or t p) = evalB e
evale e@(Iszero t) = evalB e
evale e@(Lt t p) = evalB e
evale e@(Gt t p) = evalB e
evale e@(Eq t p) = evalB e

evalInt :: Expr -> Expr
evalInt e = case evals ([(0, Void)], e) of
              (mem, I n) -> I n
              _ -> error "Requerimos obtener un entero"

evalB :: Expr -> Expr
evalB e = case evals ([(0, Void)], e) of
            (mem, B b) -> B b
            _ -> error "Requerimos obtener un booleano"

-- *****************   Test evale ***********************
evale1 = evale (Add (Mul ( I 2 ) ( I 6 ) ) (B True) )
evale2 = evale (Or (Eq (Add ( I 0 ) ( I 0 ) ) ( I 0 ) ) (Eq ( I 1 ) ( I 10 ) ) )
