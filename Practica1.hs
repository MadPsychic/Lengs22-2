module EAB where
type Variable = String


data EAB = Var Variable
         | Num Int 
         | B Bool 
         | Sum EAB EAB
         | Prod EAB EAB
         | Neg EAB
         | Pred EAB
         | Suc EAB
         | And EAB EAB
         | Or EAB EAB
         | Not EAB
         | Iszero EAB
         | Let EAB EAB
         | If EAB EAB EAB 
         | Abs Variable EAB deriving (Show, Eq)

eval1 :: EAB -> EAB
eval1 _ = error "Implementar"

evals :: EAB -> EAB
evals _ = error "Implementar"

eval :: EAB -> EAB
eval _ = error "Implementar"

data Type = () -- Definir los tipos de EAB
type Ctx = () -- Definir un sinomo para los contextos

vt :: Ctx -> EAB -> Type -> Bool
vt _ _ _ = error "Implementar"

evalt :: EAB -> EAB
evalt _ = error "Implementar"           
