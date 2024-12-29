module ModalProp where

data Prop =
        T | F
          | P String 
          | Var String
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Implies Prop Prop
          | Quant Quantifier Prop Predicate
          | Modal ModalOperator Prop
  deriving (Read, Show, Eq)

data Quantifier = Exists | Forall
  deriving (Read, Show, Eq)

type Predicate = String

data Function = Function { arguments :: [Prop], returnValue :: Prop} deriving (Show, Eq)

data VariantData = 
  VT | -- T 
  VF | -- F
  VP | -- P
  VV | -- Var
  VN | -- ...
  VA | 
  VO |
  VX | 
  VI |
  VQE | -- Quantifier Exists
  VQF | -- Quantifier Forall
  VMN | -- ModalOperator Necessarily
  VMP   -- ModalOperator Possibly

-- MODAL LOGIC

data ModalOperator = Necessarily | Possibly 
  deriving (Read, Show, Eq)

type World = [Prop]
