module Practica3 where
import Sintax

type Address = Int

type Value = Expr
type Cell = (Address, Value)
type Memory = [Cell]
