{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


module PTT.PTT where

-- WORK IN PROGRESS

import Prop (Prop(..), Quantifier(..), Predicate) 
import PTT.TH

-- Example proposition: P /\ (Q -> R)
exampleProp :: Prop
exampleProp = And (P "P") (Implies (P "Q") (P "R"))

-- Generate type.
$(makeType "ExampleType" exampleProp)
