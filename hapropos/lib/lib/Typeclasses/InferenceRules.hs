module Typeclasses.InferenceRules where

import Prop as P
import SmallProp as SP
import ModalProp as MP
import Data.Vector as V

data InferenceError
  = InvalidPremise       -- a premise is invalid
  | MissingAssumption    -- an assumption needed for the rule wasn't provided
  | RuleNotApplicable    -- the rule doesn't apply in the given context
  | OtherError           -- fallback for miscellaneous errors
  deriving (Show, Eq)

class InferenceRule prop where
  modusPonens :: prop -> prop -> Either InferenceError prop
  modusTollens :: prop -> prop -> Either InferenceError prop
  implicationIntroduction :: prop -> prop -> Either InferenceError prop
  conjunctionIntroduction :: prop -> prop -> Either InferenceError prop
  conjunctionElimination :: prop -> Either InferenceError (prop, prop)
  disjunctionIntroduction :: prop -> Either InferenceError prop
  disjunctionElimination :: prop -> Either InferenceError prop
  negationIntroduction :: (prop -> Either InferenceError prop) -> Either InferenceError prop
  negationElimination :: prop -> Either InferenceError prop
  equivalenceIntroduction :: prop -> prop -> Either InferenceError prop
  equivalenceElimination :: prop -> Either InferenceError (prop, prop)
  