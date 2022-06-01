module Practica3 where
import Sintax
import Data.List
import Data.Maybe

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

-- *****************   Test access ***********************
access1 = access 3 [ ]
access2 = access 1 [ ( 0 , B False ) , ( 2 , I 9 ) ]
access3 = access 2 [ ( 0 , I 21 ) , ( 2 , I 12 ) , ( 1 , Void) ]
access4 = access 2 [ ( 0 , I 21 ) , ( 0 , B False ) , ( 3 , Void) , ( 2 , I 12 ) ]

-- *****************   Test eval1 ***********************
evalT1 = Practica3.eval1 ( [ ( 0 , B False ) ] , (Add ( I 1 ) ( I 2 ) ) )
evalT2 = Practica3.eval1 ( [ ( 0 , B False ) ] , ( Let "x" ( I 1 ) (Add (Var "x" ) ( I 2 ) ) ) )
evalT3 = Practica3.eval1 ( [ ( 0 , B False ) ] , Assign (L 0 ) (B True) )
evalT4 = Practica3.eval1 ( [ ] , While (B True) (Add ( I 1 ) ( I 1 ) ) )
