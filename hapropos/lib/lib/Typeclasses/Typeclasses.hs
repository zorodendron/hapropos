{-# LANGUAGE MultiParamTypeClasses #-}

module Typeclasses.Typeclasses where

import qualified Prop as P
import qualified SmallProp as SP
import qualified Data.Vector as V

-- GENERAL TYPECLASSES



-- DATA TRANSFORMATIONS

class ToList a b where
  toDepthList :: a -> [b]
  toDepthVector :: a -> V.Vector b
  toContiguousDepthVector :: [a] -> V.Vector b

class ToTruthTable a b where
  toTruthTable :: a -> [b]

class ToTableau a b where
  toTableau :: a -> b

-- Once I have the tableaux set up, I can compile them into proofs in natural deduction or sequent calculus since tableaux are isomorphic to these systems.

-- NORMAL FORM TRANSFORMATIONS

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

class ToSMTLIB a where
  parseToString :: a -> String
  -- parseToAST :: a -> SMTLIBAST

class PropData a where
  propName :: a -> String


-- INSTANCES
