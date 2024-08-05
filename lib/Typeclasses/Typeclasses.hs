{-# LANGUAGE MultiParamTypeClasses #-}

module Typeclasses.Typeclasses where

-- GENERAL TYPECLASSES

import qualified Prop as P
import qualified SmallProp as SP
import qualified ModalProp as MP
import qualified Data.Vector as V

-- RULES OF INFERENCE

class InferenceRule prop where
  modusPonens :: prop -> prop -> Either String prop
  implicationElimination :: prop -> Either String (prop, prop)


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

instance InferenceRule P.Prop where
  modusPonens (P.Implies p q) r =
    if p == r then Right q
    else Left "The antecedent does not match."
  modusPonens _ _ = Left "The first proposition is not an implication."

  implicationElimination (P.Implies p q) = Right (p, q)
  implicationElimination _ = Left "Implication elimination can only be applied to implications (Implies Prop Prop)."

instance ToList P.Prop P.VariantData where
  toDepthList P.T             = [P.VT]
  toDepthList P.F             = [P.VF]
  toDepthList (P.P p)         = [P.VP] 
  toDepthList (P.Var v)       = [P.VV]
  toDepthList (P.Not p)       = [P.VN] ++ toDepthList p 
  toDepthList (P.And p q)     = [P.VA] ++ toDepthList p ++ toDepthList q
  toDepthList (P.Or p q)      = [P.VO] ++ toDepthList p ++ toDepthList q
  toDepthList (P.Xor p q)     = [P.VX] ++ toDepthList p ++ toDepthList q
  toDepthList (P.Implies p q) = [P.VI] ++ toDepthList p ++ toDepthList q
  toDepthList (P.Quant q p predicate) = 
    case q of  
      P.Exists -> [P.VQE] 
      P.Forall -> [P.VQF] 

  toDepthVector P.T             = V.singleton P.VT
  toDepthVector P.F             = V.singleton P.VF
  toDepthVector (P.P p)         = V.singleton P.VP
  toDepthVector (P.Var v)       = V.singleton P.VV
  toDepthVector (P.Not p)       = V.singleton P.VN V.++ toDepthVector p
  toDepthVector (P.And p q)     = V.singleton P.VA V.++ toDepthVector p V.++ toDepthVector q
  toDepthVector (P.Or p q)      = V.singleton P.VO V.++ toDepthVector p V.++ toDepthVector q
  toDepthVector (P.Xor p q)     = V.singleton P.VX V.++ toDepthVector p V.++ toDepthVector q
  toDepthVector (P.Implies p q) = V.singleton P.VI V.++ toDepthVector p V.++ toDepthVector q
  toDepthVector (P.Quant q s t) =
    case q of
      P.Exists -> V.singleton P.VQE V.++ toDepthVector s V.++ toDepthVector (P.Var t)
      P.Forall -> V.singleton P.VQF V.++ toDepthVector s V.++ toDepthVector (P.Var t)

  toContiguousDepthVector propositionVector = V.concat $ map toDepthVector propositionVector 


instance ToList SP.SmallProp SP.VariantData where
  toDepthList SP.T             = [SP.VT] 
  toDepthList SP.F             = [SP.VF]
  toDepthList (SP.P p)         = [SP.VP]
  toDepthList (SP.Not p)       = [SP.VN] ++ toDepthList p
  toDepthList (SP.And p q)     = [SP.VA] ++ toDepthList p ++ toDepthList q
  toDepthList (SP.Or p q)      = [SP.VO] ++ toDepthList p ++ toDepthList q
  toDepthList (SP.Xor p q)     = [SP.VX] ++ toDepthList p ++ toDepthList q
  toDepthList (SP.Implies p q) = [SP.VI] ++ toDepthList p ++ toDepthList q

  toDepthVector SP.T             = V.singleton SP.VT 
  toDepthVector SP.F             = V.singleton SP.VF
  toDepthVector (SP.P p)         = V.singleton SP.VP V.++ toDepthVector p
  toDepthVector (SP.Not p)       = V.singleton SP.VN V.++ toDepthVector p
  toDepthVector (SP.And p q)     = V.singleton SP.VA V.++ toDepthVector p V.++ toDepthVector q
  toDepthVector (SP.Or p q)      = V.singleton SP.VO V.++ toDepthVector p V.++ toDepthVector q
  toDepthVector (SP.Xor p q)     = V.singleton SP.VX V.++ toDepthVector p V.++ toDepthVector q
  toDepthVector (SP.Implies p q) = V.singleton SP.VI V.++ toDepthVector p V.++ toDepthVector q

instance ToList MP.Prop MP.VariantData where
  toDepthList MP.T             = [MP.VT]
  toDepthList MP.F             = [MP.VF]
  toDepthList (MP.P p)         = [MP.VP]
  toDepthList (MP.Var v)       = [MP.VV]
  toDepthList (MP.Not p)       = [MP.VN] ++ toDepthList p
  toDepthList (MP.And p q)     = [MP.VA] ++ toDepthList p ++ toDepthList q
  toDepthList (MP.Or p q)      = [MP.VO] ++ toDepthList p ++ toDepthList q
  toDepthList (MP.Xor p q)     = [MP.VX] ++ toDepthList p ++ toDepthList q
  toDepthList (MP.Implies p q) = [MP.VI] ++ toDepthList p ++ toDepthList q
  toDepthList (MP.Quant q s t) =
    case q of
      MP.Exists -> [MP.VQE]
      MP.Forall -> [MP.VQF]
  toDepthList (MP.Modal m p)  =
    case m of
      MP.Necessarily -> [MP.VMN]
      MP.Possibly    -> [MP.VMP]

  toDepthVector MP.T             = V.singleton MP.VT
  toDepthVector MP.F             = V.singleton MP.VF
  toDepthVector (MP.P p)         = V.singleton MP.VP
  toDepthVector (MP.Var v)       = V.singleton MP.VV
  toDepthVector (MP.Not p)       = V.singleton MP.VN V.++ toDepthVector p
  toDepthVector (MP.And p q)     = V.singleton MP.VA V.++ toDepthVector p V.++ toDepthVector q
  toDepthVector (MP.Or p q)      = V.singleton MP.VO V.++ toDepthVector p V.++ toDepthVector q
  toDepthVector (MP.Xor p q)     = V.singleton MP.VX V.++ toDepthVector p V.++ toDepthVector q
  toDepthVector (MP.Implies p q) = V.singleton MP.VI V.++ toDepthVector p V.++ toDepthVector q
  toDepthVector (MP.Quant q s t) =
    case q of
      MP.Exists -> V.singleton MP.VQE V.++ toDepthVector s V.++ toDepthVector (MP.Var t)
      MP.Forall -> V.singleton MP.VQF V.++ toDepthVector s V.++ toDepthVector (MP.Var t)
  toDepthVector (MP.Modal m p)   =
    case m of 
      MP.Necessarily -> V.singleton MP.VMN
      MP.Possibly    -> V.singleton MP.VMP
