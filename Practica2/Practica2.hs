module Practica2 where
import Sintax
import Data.List

type Identifier = Int
data Type = T Practica2.Identifier
          | Integer | Boolean
          | Arrow Type Type
          deriving (Eq)

instance Show Type where
  show e = case e of
    (T i) -> "T " ++ show i
    (Arrow e t) -> show e ++ " -> " ++ show t
    Integer -> "Int"
    Boolean -> "Bool"

type Ctxt = [(Sintax.Identifier, Type)]
type Constraint = [(Type, Type)]

-- * Algoritmo de inferencia de tipos
-- * Tipado con restricciones

tvars :: Type -> [Practica2.Identifier]
tvars (T s) = [s]
tvars Integer = []
tvars Boolean = []
tvars (Arrow a b) = nub(tvars a ++ tvars b)


fresh' :: [Type] -> [Int]
fresh' [] = []
fresh' (x:xs) = nub((tvars x) ++ fresh'(xs)) 

fresh'' :: [Int] -> Int
fresh'' xs = head([0..] \\ xs)

fresh :: [Type] -> Type
fresh [] = error "No exite tipo"
fresh xs = T (fresh''(fresh' xs))


rest :: ([Type], Expr) -> ([Type], Ctxt, Type, Constraint)
rest (xs, (Var x)) = (((fxs):xs), [(x, fxs)], fxs, [])
  where fxs = fresh xs
rest (xs, (I n)) = (Integer:xs, [], Integer, [])
rest (xs, (B b)) = (Boolean:xs, [], Boolean, [])
rest (xs, Iszero e) = (xs1, g1 , Boolean, r1)
  where (xs1, g1, t1, r1) = (rest (xs, e))
rest (xs, (Succ e)) = (xs1, g1 , Integer, r1)
  where (xs1, g1, t1, r1) = (rest (xs, e))
rest (xs, (Pred e)) = (xs1, g1 , Integer, r1)
  where (xs1, g1, t1, r1) = (rest (xs, e))
rest (xs, (Not e)) = (xs1, g1 , Boolean, r1)
  where (xs1, g1, t1, r1) = (rest (xs, e))
rest (xs,(Add e1 e2)) = (xs2, g1 ++ g2, Integer, rf)
  where (xs1, g1, t1, r1) = (rest (xs, e1))
        (xs2, g2, t2, r2) = (rest (xs1, e2))
        rs = [(tn1, tn2) | (x, tn1) <- g1, (y, tn2) <- g2, x == y]
        re = [(t1, Integer),(t2, Integer)]
        rf = r1 ++ r2 ++ rs ++ re
rest (xs, (Mul e t)) = (xs2, g1 ++ g2, Integer, rf)
  where (xs1, g1, t1, r1) = (rest (xs, e))
        (xs2, g2, t2, r2) = (rest (xs1, t))
        rs = [(tn1, tn2) | (x,tn1) <- g1, (y, tn2) <- g2, x == y]
        re = [(t1, Integer), (t2, Integer)]
        rf = r1 ++ r2 ++ rs ++ re
rest (xs, (And e t)) = (xs2, g1 ++ g2, Boolean, rf)
  where (xs1, g1, t1, r1) = (rest (xs, e))
        (xs2, g2, t2, r2) = (rest (xs1, t))
        rs = [(tn1, tn2) | (x, tn1) <- g1, (y, tn2) <- g2, x == y]
        re = [(t1, Boolean), (t2, Boolean)]
        rf = r1 ++ r2 ++ rs ++ re
rest (xs, (Or e t)) = (xs2, g1 ++ g2, Boolean, rf)
  where (xs1, g1, t1, r1) = (rest (xs, e))
        (xs2, g2, t2, r2) = (rest (xs1, t))
        rs = [(tn1, tn2) | (x, tn1) <- g1, (y, tn2) <- g2, x == y]
        re = [(t1, Boolean), (t2, Boolean)]
        rf = r1 ++ r2 ++ rs ++ re
rest (xs, (Lt e t)) = (xs2, g1 ++ g2, Boolean, rf)
  where (xs1, g1, t1, r1) = (rest (xs, e))
        (xs2, g2, t2, r2) = (rest (xs1, t))
        rs = [(tn1, tn2) | (x, tn1) <- g1, (y, tn2) <- g2, x == y]
        re = [(t1, Boolean), (t2, Boolean)]
        rf = r1 ++ r2 ++ rs ++ re
rest (xs, Gt e t) = (xs2, g1 ++ g2, Boolean, rf)
  where (xs1, g1, t1, r1) = (rest (xs, e))
        (xs2, g2, t2, r2) = (rest (xs1, t))
        rs = [(tn1, tn2) | (x, tn1) <- g1, (y, tn2) <- g2, x == y]
        re = [(t1, Boolean), (t2, Boolean)]
        rf = r1 ++ r2 ++ rs ++ re
rest (xs, (App e t)) = (((fxs):xs), g1 ++ g2, Integer, rf)
  where (xs1, g1, t1, r1) = (rest (xs, e))
        (xs2, g2, t2, r2) = (rest (xs1, t))
        rs = [(tn1, tn2) | (x, tn1) <- g1, (y, tn2) <- g2, x == y]
        re = [(t1, Integer), (t2, Integer)]
        rf = r1 ++ r2 ++ rs ++ re
        fxs = fresh xs
rest (xs, (Let i e t)) = (xs2 ++ [x], g1 ++ g2 ++ [(i, x)], t2, rf)
  where (xs1, g1, t1, r1) = (rest (xs, e))
        (xs2, g2, t2, r2) = (rest (xs1, t))
        rs = [(tn1, tn2) | (x, tn1) <- g1, (y, tn2) <- g2, x == y]
        x = fresh xs2
        rf = r1 ++ r2 ++ rs ++ [(t1, x)]
rest (xs, (If e1 e2 e3)) = (xs3, g1 ++ g2 ++ g3, t1, r1 ++ r2 ++ r3 ++ rt1 ++ rt2 ++ rt3 ++ [(t2, t3), (t1, Boolean)])
  where (xs1, g1, t1, r1) = (rest (xs, e1))
        (xs2, g2, t2, r2) = (rest ([t1], e2))
        (xs3, g3, t3, r3) = (rest ([t2], e3))
        rt1 = [(s2, s3) | (x, s2) <- g2, (y, s3) <- g1, x == y]
        rt2 = [(s2, s3) | (x, s2) <- g2, (y, s3) <- g3, x == y]
        rt3 = [(s2, s3) | (x, s2) <- g1, (y, s3) <- g3, x == y]

-- * Algoritmo de unificación

type Substitution = [(Practica2.Identifier, Type)]

subst :: Type -> Substitution -> Type
subst t [] = t
subst (T n) ((i,t):xs) = if n == i
                           then  t
                           else subst (T n) xs
subst (Arrow t1 t2) xs = Arrow (subst t1 xs) (subst t2 xs)
subst t _ = t

comp :: Substitution -> Substitution -> Substitution
comp s1 s2 = noDup ((map (\x -> (fst x, subst (snd x) s2 )) s1) ++ s2)

noDup :: (Eq a) => [(a, b)] -> [(a, b)]
noDup (x:xs) = x : noDup (filter (\y -> (fst y) /= (fst x)) xs)
noDup [] = []

substC :: Substitution -> Constraint -> Constraint
substC _ [] = []
substC u ((t , s):xs) =  [((subst t u) , (subst s u))] ++ substC u xs

unif :: Constraint -> Substitution
unif [] = []
unif ((t, s):xs)
  | t == s = unif xs
  | otherwise = case (t,s) of
      (T x, t) -> if x `elem` (tvars t)
                  then error "Unificación imposible"
                  else comp (unif (substC [(x, t)] xs)) ([(x, t)])
      (t, T x) -> unif ((T x, t):xs)
      (Arrow t1 t2, Arrow s1 s2) -> unif ([(t1, s1),(t2, s2)] ++ xs)
      (_, _) -> error "Tipo no unificable"

-- * Inferencia de tipos

infer :: Expr -> (Ctxt, Type)
infer (Var x) = (u , subst t (unif r))
  where (lt, u, t, r ) = rest ([], Var x)
infer (I n) = ([], Integer)
infer (B b) = ([], Boolean)
infer (Succ e) = (u, subst t (unif r))
  where (lt, u, t, r) = rest ([Integer], Succ e)
infer (Pred e) = (u, subst t (unif r))
  where (lt, u, t, r) = rest ([Integer], Pred e)
infer (Not e) = (u, subst t (unif r))
  where (lt, u, t, r) = rest ([Boolean], Not e)
infer (Iszero e) = (u, subst t (unif r))
  where (lt, u, t, r) = rest ([Boolean], Iszero e)
infer (Add e1 e2) = (u, subst t (unif r))
  where (lt, u, t, r) = rest ([Integer],  Add e1 e2)
infer (Mul e1 e2) = (u, subst t (unif r))
  where (lt, u, t, r) = rest ([Integer],  Mul e1 e2)
infer (And e1 e2) = (u, subst t (unif r))
  where (lt, u, t, r ) = rest ([Boolean],  And e1 e2)
infer (Or e1 e2) = (u, subst t (unif r))
  where (lt, u, t, r ) = rest ([Boolean],  Or e1 e2)
