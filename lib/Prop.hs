module Prop where

data Prop = 
        T | F 
          | P String -- Another proposition
          | Var String
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Xor Prop Prop
          | Implies Prop Prop
          | Quant Quantifier Prop Predicate
  deriving (Read, Show, Eq)

data Quantifier = Exists | Forall
  deriving (Read, Show, Eq) 

type Predicate = String

type Function = (Prop, Prop)
