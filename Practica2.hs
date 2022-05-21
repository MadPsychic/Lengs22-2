module Sintax where

type Identifier = Int
data Type = T Identifier
          | Integer | Boolean
          | Arrow Type Type

instance Show Type where
  show e = case e of
    (T t) -> "T " ++ show t
    (Arrow e t) -> show e ++ " -> " ++ show t
    Integer -> "int"
    Boolean -> "bool"

type Ctxt = [(Identifier, Type)]
type Constraint = [(Type, Type)]

{--
 -- Sintaxis Practica1
 -- Para su Practica 2 deben modificar esto (es lo unico que necesitan de su P1)
 -- Agregar y eliminar las cosas que sean necesarias segun la descripcion que se
 -- dio en la especificacionde la practica.
 --}
data Expr = Var Identifier | I Int | B Bool
          | Fn Identifier Expr
          | Succ Expr | Pred Expr
          | Add Expr Expr | Mul Expr Expr
          | Not Expr | Iszero Expr
          | And Expr Expr | Or Expr Expr
          | Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
          | If Expr Expr Expr
          | Let Identifier Expr Expr
          | App Expr Expr
          deriving (Show, Eq)

-- * Algoritmo de inferencia de tipos

tvars :: Type -> [Identifier]
tvars _ = error "UwU"

fresh :: [Type] -> Type
fresh _ = error "UwU"

rest :: ([Type], Expr) -> ([Type], Ctxt, Type, Constraint)
rest _ = error "OwO"

-- * Algoritmo de unificación

type Substitution = [(Identifier, Type)]

subst :: Type -> Substitution -> Type
subst = error "UwO"

comp :: Substitution -> Substitution -> Substitution
comp = error "OwU"

unif :: Constraint -> Substitution
unif = error "meh"

-- * Inferencia de tipos

infer :: Expr -> (Ctxt, Type)
infer = error "bruh i'm out of error names"
