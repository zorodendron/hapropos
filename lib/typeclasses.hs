{-# LANGUAGE MultiParamTypeClasses #-}

module Typeclasses where

import qualified Prop
import qualified SmallProp

class ToList a b where
  toDepthList :: a -> [b]

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

instance ToList Prop.Prop Prop.VariantData where
  toDepthList Prop.T             = [Prop.VT]
  toDepthList Prop.F             = [Prop.VF]
  toDepthList (Prop.P p)         = [Prop.VP] 
  toDepthList (Prop.Var v)       = [Prop.VV]
  toDepthList (Prop.Not p)       = [Prop.VN] ++ toDepthList p 
  toDepthList (Prop.And p q)     = [Prop.VA] ++ toDepthList p ++ toDepthList q
  toDepthList (Prop.Or p q)      = [Prop.VO] ++ toDepthList p ++ toDepthList q
  toDepthList (Prop.Xor p q)     = [Prop.VX] ++ toDepthList p ++ toDepthList q
  toDepthList (Prop.Implies p q) = [Prop.VI] ++ toDepthList p ++ toDepthList q
  toDepthList (Prop.Quant q s t) = 
    case q of 
      Prop.Exists -> [Prop.VQE] -- Would recursion be good? 
      Prop.Forall -> [Prop.VQF] 

instance ToList SmallProp.SmallProp SmallProp.VariantData where
  toDepthList SmallProp.T             = [SmallProp.VT] 
  toDepthList SmallProp.F             = [SmallProp.VF]
  toDepthList (SmallProp.P p)         = [SmallProp.VP]
  toDepthList (SmallProp.Not p)       = [SmallProp.VN] ++ toDepthList p
  toDepthList (SmallProp.And p q)     = [SmallProp.VA] ++ toDepthList p ++ toDepthList q
  toDepthList (SmallProp.Or p q)      = [SmallProp.VO] ++ toDepthList p ++ toDepthList q
  toDepthList (SmallProp.Xor p q)     = [SmallProp.VX] ++ toDepthList p ++ toDepthList q
  toDepthList (SmallProp.Implies p q) = [SmallProp.VI] ++ toDepthList p ++ toDepthList q
