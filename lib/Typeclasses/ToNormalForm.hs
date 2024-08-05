module Typeclasses.ToNormalForm where

class ToNormalForm a where
  toConjunctiveNormalForm :: a -> a
  toDisjunctiveNormalForm :: a -> a
  toNegationNormalForm :: a -> a
  toPrenexNormalForm :: a -> a
  toSkolemNormalForm :: a -> a
  toClauseNormalForm :: a -> a
  toLinearNormalForm :: a -> a
  toHornNormalForm :: a -> a
  toKromNormalForm :: a -> a
