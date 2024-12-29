module LiquidProp where

-- WORK IN PROGRESS

import Prop

{-@ data Prop 
    = T 
    | F 
    | P {pname :: String} 
    | Var {vname :: String} 
    | Not {nprop :: Prop} 
    | And {leftProp :: Prop, rightProp :: Prop} 
    | Or {leftProp :: Prop, rightProp :: Prop} 
    | Implies {antecedent :: Prop, consequent :: Prop} 
    | Quant {qtype :: Quantifier, qprop :: Prop, qpred :: Predicate} 
@-}

{-@ data Quantifier = Exists | Forall @-}

-- Predicate is just an alias for String here, but let's define it in terms of LH as well.
-- Since LH doesn't support refinement types for aliases directly, we just document it.

type Predicate = String

-- Using LH. Todo: make with TemplateHaskell.
{-@ measure isPropNot @-}
--isPropNot :: Prop -> Bool
isPropNot (Not _) = True
isPropNot _       = False

{-@ measure measure_isDN @-}
isDN :: Prop -> Bool
isDN (Not (Not _)) = True
isDN _             = False

{-@ die :: {v:String | false} -> a  @-}
die :: String -> a
die msg = error msg

{-@ type Negation = {v:Prop | isPropNot v} @-}
{-@ type DoubleNegation = {v:Prop | measure_isDN v} @-}

--{-@ safeDNE :: {p:Prop | isPropNot p} -> Prop @-}
{-@ safeDNE :: DoubleNegation -> Prop @-}
safeDNE :: Prop -> Prop
safeDNE (Not (Not x)) = x
safeDNE _              = die "Incorrect proposition as argument."
