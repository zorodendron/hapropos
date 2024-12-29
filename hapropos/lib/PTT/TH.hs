{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- WORK IN PROGRESS

module PTT.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Prop (Prop(..), Quantifier(..))


-- Represents false propositions.
data VoidType deriving Show

-- Helper data-type for Xor.
data XorType p q = XorL p | XorR q deriving Show

-- GADT to represent existential quantification.
data ExistsType where
    ExistsType :: (forall a. p a) -> ExistsType

-- TYPE-CLASSES

instance Show ExistsType where
    show (ExistsType _) = "Exists"

-- FUNCTIONS
propToType :: Prop -> Q Type
propToType (P.TruthValue True) = [t| () |]
propToType (P.TruthValue False) = [t| VoidType |]
propToType (P name) = varT (mkName name)
propToType (Var name) = varT (mkName name)
propToType (Not p) = [t| $(propToType p) -> VoidType |]
propToType (And p q) = [t| ($(propToType p), $(propToType q)) |]
propToType (Or p q) = [t| Either $(propToType p) $(propToType q) |]
propToType (Implies p q) = [t| $(propToType p) -> $(propToType q) |]
propToType (Quant Forall p _) = [t| forall a. $(propToType p) |]
propToType (Quant Exists p _) = [t| ExistsType $(propToType p) |]

-- Function to generate a type (important).
makeType :: String -> Prop -> Q [Dec]
makeType typeNameStr prop = do
    let typeName = mkName typeNameStr
    typ <- propToType prop
    return [TySynD typeName [] typ]

printTH :: Language.Haskell.TH.Ppr.Ppr a => Language.Haskell.TH.Syntax.Q a -> IO ()
printTH a = runQ a >>= putStrLn . pprint
