{-# LANGUAGE TemplateHaskell #-} 

module PTT.Named where

import Language.Haskell.TH

import Prop

type TrueType = ()
data FalseType deriving Show

data PropositionType p = PropositionType p
data VarType name = VarType name
data NotType p = NotType p
data AndType p q = AndType p q
data OrType p q = OrType p q
data XorType p q = XorType p q
data ImpliesType p q = ImpliesType p q
data QuantType quantifier thing predicate = QuantType quantifier thing predicate

data ExistsType thing predicate = ExistsType thing predicate
data ForAllType thing predicate = ForAllType thing predicate

-- FUNCTIONS
-- this is an unfinished modified version of the "unnamed" compilation code
propToType :: Prop -> Q Type
propToType T = [t| TrueType |]
propToType F = [t| FalseType |]
propToType (P name) = [t| PropositionType $(litT (strTyLit name)) |]-- propToType (P name) = [t| PropositionType $(propToType name) |]
propToType (Var name) = [t| VarType $(litT (strTyLit name)) |]-- propToType (Var name) = [t| VarType $(propToType name) |]
propToType (Or p q) = [t| OrType $(propToType p) $(propToType q) |]
propToType (Xor p q) = [t| XorType $(propToType p) $(propToType q) |]
propToType (Implies p q) = [t| ImpliesType $(propToType p) $(propToType q) |]
propToType (Quant Forall thing predicate) = 
  [t| ForAllType $(propToType thing) $(conT (mkName predicate)) |]
propToType (Quant Exists thing predicate) = 
  [t| ExistsType $(propToType thing) $(conT (mkName predicate)) |]

makeType :: String -> Prop -> Q [Dec]
makeType typeNameStr prop = do
    let typeName = mkName typeNameStr
    typ <- propToType prop
    return [TySynD typeName [] typ]
