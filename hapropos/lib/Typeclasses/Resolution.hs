module Typeclasses.Resolution where

import Prop as P

data ResolutionError =
    NoncomplementaryLiterals
  | InvalidClauseStructure
  | CircularResolution
  | UnsatButNoEmptyClause
  | PrematureTermination
  | EmptyClauseFromTautologies
  | QuantifierError
  | UnsupportedOperator

class Resolution prop where
  resolution :: prop -> prop -> [prop] -> Either ResolutionError prop -- resolution assumes that both props are in CNF

instance Resolution Prop where
  resolution p1 p2 = case isContradictory p1 p2 of
    True ->
