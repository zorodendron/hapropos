module ModalProp where

data Prop =
        T | F
          | P String -- Another proposition.
          | Var String
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Xor Prop Prop
          | Implies Prop Prop
          | Quant Quantifier Prop Predicate
          | Modal ModalOperator Prop
  deriving (Read, Show, Eq)

data Quantifier = Exists | Forall
  deriving (Read, Show, Eq)

type Predicate = String

data Function = Function { arguments :: [Prop], returnValue :: Prop} deriving (Show, Eq)

-- MODAL LOGIC
data ModalOperator = Necessarily | Possibly

type World = [Prop]
