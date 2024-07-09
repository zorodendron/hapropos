{-# LANGUAGE MultiParamTypeClasses #-}

module Typeclasses where

import qualified Prop as P
import qualified SmallProp as SP
import qualified Data.Vector as V

class ToList a b where
  toDepthList :: a -> [b]
  toDepthVector :: a -> V.Vector b

class ToTruthTable a b where
  toTruthTable :: a -> [b]  
  
class ToTableau a b where 
  toTableau :: a -> b

-- Once I have the tableaux set up, I can compile them into proofs in natural deduction or sequent calculus since tableaux are isomorphic to these systems.

class ToSMTLIB a where
  parseToString :: a -> String
  -- parseToAST :: a -> SMTLIBAST

class PropData a where
  propName :: a -> String  


-- INSTANCES

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
  toDepthList (P.Quant q s t) = 
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


instance ToList SP.SmallProp SP.VariantData where
  toDepthList SP.T             = [SP.VT] 
  toDepthList SP.F             = [SP.VF]
  toDepthList (SP.P p)         = [SP.VP]
  toDepthList (SP.Not p)       = [SP.VN] ++ toDepthList p
  toDepthList (SP.And p q)     = [SP.VA] ++ toDepthList p ++ toDepthList q
  toDepthList (SP.Or p q)      = [SP.VO] ++ toDepthList p ++ toDepthList q
  toDepthList (SP.Xor p q)     = [SP.VX] ++ toDepthList p ++ toDepthList q
  toDepthList (SP.Implies p q) = [SP.VI] ++ toDepthList p ++ toDepthList q

  
