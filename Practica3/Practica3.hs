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

