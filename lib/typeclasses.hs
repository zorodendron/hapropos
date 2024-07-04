{-# LANGUAGE MultiParamTypeClasses #-}

module Typeclasses where

import Prop

class ToList a b where
  toDepthList :: a -> [b]

class ToSMTLIB a where
  parseToString :: a -> String
  -- parseToAST :: a -> SMTLIBAST

class PropData a where
  propName :: a -> String  


-- INSTANCES

instance ToList Prop VariantData where
  toDepthList T             = [VT]
  toDepthList F             = [VF]
  toDepthList (P p)         = [VP] 
  toDepthList (Var v)       = [VV]
  toDepthList (Not p)       = [VN] ++ toDepthList p 
  toDepthList (And p q)     = [VA] ++ toDepthList p ++ toDepthList q
  toDepthList (Or p q)      = [VO] ++ toDepthList p ++ toDepthList q
  toDepthList (Xor p q)     = [VX] ++ toDepthList p ++ toDepthList q
  toDepthList (Implies p q) = [VI] ++ toDepthList p ++ toDepthList q
