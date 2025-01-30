module Typeclasses.NormalForms where

import Prop as P
import SmallProp as SP

class ToNormalForm prop where
  eliminateImplications :: prop -> prop
  eliminateImplications = id -- default: no-op for types that don't need it

  toConjunctiveNormalForm :: prop -> prop
  toDisjunctiveNormalForm :: prop -> prop
  toNegationNormalForm :: prop -> prop
  toPrenexNormalForm :: prop -> prop
  toSkolemNormalForm :: prop -> prop
  toClauseNormalForm :: prop -> prop
  toLinearNormalForm :: prop -> prop
  toHornNormalForm :: prop -> prop
  toKromNormalForm :: prop -> prop
  distributeOr :: prop -> prop

instance ToNormalForm P.Prop where

  eliminateImplications :: P.Prop -> P.Prop
  eliminateImplications (P.TruthValue b) = P.TruthValue b
  eliminateImplications (P.Ter t)           = P.Ter t
  eliminateImplications (P.Pre name terms)  = P.Pre name terms
  eliminateImplications (P.Not p)           = P.Not (eliminateImplications p)
  eliminateImplications (P.And p1 p2)       = P.And (eliminateImplications p1) (eliminateImplications p2)
  eliminateImplications (P.Or p1 p2)        = P.Or (eliminateImplications p1) (eliminateImplications p2)
  eliminateImplications (P.Implies p1 p2)   = P.Or (P.Not (eliminateImplications p1)) (eliminateImplications p2)
  eliminateImplications (P.Quant quant s p) = P.Quant quant s (eliminateImplications p)

  toConjunctiveNormalForm :: P.Prop -> P.Prop
  toConjunctiveNormalForm p = distributeOr $ toNegationNormalForm $ eliminateImplications p

  toNegationNormalForm :: P.Prop -> P.Prop
  toNegationNormalForm (P.TruthValue b)             = P.TruthValue b
  toNegationNormalForm (P.Ter t)                    = P.Ter t
  toNegationNormalForm (P.Not (P.TruthValue True))  = toNegationNormalForm (P.TruthValue False)
  toNegationNormalForm (P.Not (P.TruthValue False)) = toNegationNormalForm (P.TruthValue True)
  toNegationNormalForm (P.Not (P.And p q))          = P.Or (P.Not (toNegationNormalForm p)) (P.Not (toNegationNormalForm q))
  toNegationNormalForm (P.Not (P.Or p q))           = P.And (P.Not (toNegationNormalForm p)) (P.Not (toNegationNormalForm q))
  toNegationNormalForm (P.Not (Quant Exists s p))   = Quant Forall s (toNegationNormalForm (P.Not p))
  toNegationNormalForm (P.Not (Quant Forall s p))   = Quant Exists s (toNegationNormalForm (P.Not p))
  toNegationNormalForm (P.Not (P.Not p))            = toNegationNormalForm p
  toNegationNormalForm (P.Not x) = P.Not (toNegationNormalForm x)
  toNegationNormalForm (P.And (P.Implies p q) (P.Implies q' p'))
    | p == p' && q == q' =
        P.And
          (P.Or (toNegationNormalForm (P.Not p)) (toNegationNormalForm q))
          (P.Or (toNegationNormalForm (P.Not q)) (toNegationNormalForm p))
    | otherwise = P.And (toNegationNormalForm (P.Implies p q)) (toNegationNormalForm (P.Implies q' p'))
  toNegationNormalForm (P.And p q)                  = P.And (toNegationNormalForm p) (toNegationNormalForm q)
  toNegationNormalForm (P.Or p q)                   = P.Or (toNegationNormalForm p) (toNegationNormalForm q)
  toNegationNormalForm (P.Implies p q)              = P.Or (P.Not (toNegationNormalForm p)) (toNegationNormalForm q)

  distributeOr :: P.Prop -> P.Prop
  distributeOr (P.And p q)          = P.And (distributeOr p) (distributeOr q)
  distributeOr (P.Or p (P.And q r)) =
    P.And (distributeOr (P.Or p q)) (distributeOr (P.Or p r))
  distributeOr (P.Or (P.And q r) p) =
    P.And (distributeOr (P.Or q p)) (distributeOr (P.Or r p))
  distributeOr (P.Or p q)           = P.Or (distributeOr p) (distributeOr q)
  distributeOr p                    = p -- base case: return atomic propositions or other non-`Or`/`And` nodes as-is
