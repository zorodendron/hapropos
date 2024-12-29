module HOL where

data Prop = T 
  | F 
  | Var String
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Implies Prop Prop
  | Quant Quantifier Prop Predicate
  | Rel Relation       
  deriving (Read, Show, Eq)

data Quantifier = Exists | Forall
  deriving (Read, Show, Eq) 

type Predicate = String

type Relation = [Relatum]

data Relatum = RelatumRelation Relation | RelatumProp Prop | RelatumFunction Function

type Function = Prop -> Prop
