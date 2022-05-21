module Practica2 where
import Sintax

type Identifier = Int
data Type = T Practica2.Identifier
          | Integer | Boolean
          | Arrow Type Type

instance Show Type where
  show e = case e of
    (T t) -> "T " ++ show t
    (Arrow e t) -> show e ++ " -> " ++ show t
    Integer -> "int"
    Boolean -> "bool"

type Ctxt = [(Sintax.Identifier, Type)]
type Constraint = [(Type, Type)]

-- * Algoritmo de inferencia de tipos

tvars :: Type -> [Practica2.Identifier]
tvars _ = error "UwU"

fresh :: [Type] -> Type
fresh _ = error "UwU"

rest :: ([Type], Expr) -> ([Type], Ctxt, Type, Constraint)
rest _ = error "OwO"

-- * Algoritmo de unificación

type Substitution = [(Practica2.Identifier, Type)]

subst :: Type -> Substitution -> Type
subst = error "UwO"

comp :: Substitution -> Substitution -> Substitution
comp = error "OwU"

unif :: Constraint -> Substitution
unif = error "meh"

-- * Inferencia de tipos

infer :: Expr -> (Ctxt, Type)
infer = error "bruh i'm out of error names"
