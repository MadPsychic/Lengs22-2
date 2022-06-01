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
  else if contains n (listMemory xs) then Just (eval1((listValue xs) !!(n-1)))
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
             | distintos (listMemory xs) =error "Corrupted memory"
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



--- +++++++++++++++++++++ Test frVars ++++++++++++++++++++++++
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







--- ******************** Test subst ************************
subst1= subst (Add (Var "x" ) ( I 5 ) ) ( "x" , I 10 )
subst2= subst ( Let "x" ( I 1 ) (Var "x" ) ) ( "y" , Add (Var "x" ) ( I 5 ) )
subst3= subst (Assign (L 2 ) (Add ( I 0 ) (Var "z" ) ) ) ("z" , B False )
