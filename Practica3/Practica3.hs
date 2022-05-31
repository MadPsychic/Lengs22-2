module Practica3 where
import Sintax
import Data.List

type Address = Int

type Value = Expr
type Cell = (Address, Value)
type Memory = [Cell]

{--
 -- Dada una memoria, genera una nueva direccion de memoria
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
 -- Dada una dreccion de memoria, devuelve el valor contenido en la celda con tal 
 -- direccion, en caso de no encontrarla regresara Nothing
 --}

contains :: Eq a => a -> [a] -> Bool
contains = \elem -> \myList ->
  case myList of
    [] -> False 
    x:xs | x == elem -> True 
    _:xs -> contains elem xs 


access ::  Address -> Memory -> Maybe Value
access a (x:xs) | eval1 (Dref (snd x)) = let e = access snd x



-- *****************   Test access ***********************
access1 = access 3 [ ]


