module Prop where

data Prop
  = TruthValue Bool        -- truth values (True, False)
  | Ter Term               -- terms
  | Pre String [Term]      -- predicate with terms (the String represents the predicate name and [Term] is a list of terms as arguments.)
  | Not Prop               -- negation
  | And Prop Prop          -- conjunction
  | Or Prop Prop           -- disjunction
  | Implies Prop Prop      -- implication
  | Quant Quantifier String Prop -- quantification with a quantifier, variable, and a predicate
  deriving (Read, Show, Eq, Ord)


data Term
  = Const String         -- constants (e.g., "Earth")
  | Var String           -- variables (e.g., "x", "y")
  | Func String [Term]   -- functions with terms as arguments (e.g., f(x, y))
  deriving (Eq, Show, Read, Ord)

data Quantifier = Exists | Forall
  deriving (Read, Show, Eq, Ord)



occurs :: Term -> Term -> Bool
occurs x y
  | x == y = True
occurs x (Func _ args) = any (occurs x) args
occurs _ _             = False
