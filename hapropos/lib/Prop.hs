module Prop where

data Prop
  = TruthValue Bool        -- truth values (True, False)
  | Ter Term               -- terms
  | Pre String [Term]      -- predicate with terms
  | Not Prop               -- negation
  | And Prop Prop          -- conjunction
  | Or Prop Prop           -- disjunction
  | Implies Prop Prop      -- implication
  | Quant Quantifier String Prop -- quantification with a quantifier, variable, and a predicate
  deriving (Read, Show, Eq)

data Term
  = Const String         -- constants (e.g., "Earth")
  | Var String           -- variables (e.g., "x", "y")
  | Func String [Term]   -- functions with terms as arguments (e.g., f(x, y))
  deriving (Eq, Show, Read)

data Quantifier = Exists | Forall
  deriving (Read, Show, Eq)

data VariantData =
  VT | -- T
  VF | -- F
  VC | -- Const
  VV | -- Var
  VN | -- ...
  VA |
  VO |
  VX |
  VI |
  VQE | -- Quantifier Exists
  VQF   -- Quantifier Forall
