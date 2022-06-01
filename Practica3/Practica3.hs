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
-- subst. Extiende esta función para las nuevas expresiones.
--}

subst :: Expr -> Substitution -> Expr
subst (Var x) (a,b) = if x == a
                      then b
                      else Var x
subst (I x) (a,b) = if x == a 
  then b
  else I x
subst (B x) (a,b) = if x == a 
  then b
  else B x
subst (L x ) (a,b) = if x == a 
  then b
  else I x
subst Add x y =  Add (subst x) (subst y)
subst Mul x y = Mul (subst x) (subst y)
subst Succ x = Succ (subst x)
subst Pred x = Pred (subst x)
subst And x y = And (subst x) (subst y)
subst Or x y = Or(subst x) (subst y)
subst Not x = Not(subst x)
subst Iszero x = Iszero(subst x)
subst Lt x y = Lt (subst x) (subst y)
subst Gt x y = Gt (subst x) (subst y)
subst Eq x y = Eq (subst x) (subst y)
subst If x y z= If (subst x) (subst y) (subst z)
subst Let x y z = Let x (subst y) (subst z)
subst Fn x y = Fn x (subst y)
subst App x y = App (subst x) (subst y)
subst Alloc x = Alloc (subst x)
subst Dref x = Dref (subst x)
subst Assign x y = Assign (subst x) (subst y)
subst Void = Void
subst Seq x y= Seq (subst x) (subst y)
subst While x y = While (subst x) (subst y)
subst Abs x y = x (subst y)







--- ******************** Test subst ************************
subst1= subst (Add (Var "x" ) ( I 5 ) ) ( "x" , I 10 )
subst2= subst ( Let "x" ( I 1 ) (Var "x" ) ) ( "y" , Add (Var "x" ) ( I 5 ) )
subst3= subst (Assign (L 2 ) (Add ( I 0 ) (Var "z" ) ) ) ("z" , B False )
