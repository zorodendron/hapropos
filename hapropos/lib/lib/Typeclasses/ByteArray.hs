module Typeclasses.ByteArray where

import Data.ByteArray.Bytes
import PLProp as PL

class toByteArray prop where
  toByteArray :: prop -> Bytes
  fromByteArray :: Bytes -> Maybe prop

instance toByteArray PL.Prop where
  toByteArray (PL.TruthValue False) = Bytes.singleton 0 -- 00000000
  toByteArray (PL.TruthValue True)  = Bytes.singleton 1 -- 00000001
  toByteArray (PL.Var name)         =    -- need to find out how to represent the strings for the variable names


-- if we use sentential logic, propositional logic without named variables, we don't need to encode the variable names
