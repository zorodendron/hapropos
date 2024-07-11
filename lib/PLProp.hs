module PLProp where

import Data.List (transpose)
import Control.Monad (replicateM)

-- Propositional Logic

data PLProp =
  T | F
    | P String
    | Not PLProp
    | And PLProp PLProp
    | Or PLProp PLProp
    | Xor PLProp PLProp
    | Implies PLProp PLProp
    deriving (Show, Eq)

data VariantData = VT | VF | VP | VN | VA | VO | VX | VI

-- Collect all atomic propositions in a proposition.
collectProps :: PLProp -> [String]
collectProps T             = []
collectProps F             = []
collectProps (P p)         = [p]
collectProps (Not p)       = collectProps p
collectProps (And p q)     = collectProps p ++ collectProps q
collectProps (Or p q)      = collectProps p ++ collectProps q
collectProps (Xor p q)     = collectProps p ++ collectProps q
collectProps (Implies p q) = collectProps p ++ collectProps q

data VariableColumn      = VariableColumn      { columnVar :: String, vcBooleans :: [Bool] } deriving Show
data TruthFunctionColumn = TruthFunctionColumn { truthFN :: PLProp, tfcBooleans :: [Bool] } deriving Show
data TruthTable          = TruthTable          { variableColumns :: [VariableColumn], truthFunctionColumns :: [TruthFunctionColumn] } deriving Show

variableColumnLookup :: String -> [VariableColumn] -> Maybe VariableColumn
variableColumnLookup key [] = Nothing
variableColumnLookup key (x:xs) | (columnVar x) == key = Just x
                                | otherwise = variableColumnLookup key xs

makeVColumns :: [String] -> [[Bool]] -> [VariableColumn]
makeVColumns varNames rows = zipWith (\n -> \r -> VariableColumn {columnVar = n, vcBooleans = r}) varNames (transpose rows)

makeTFColumn :: PLProp -> [VariableColumn] -> TruthFunctionColumn
makeTFColumn truthFunction varColumns =
  TruthFunctionColumn { truthFN = truthFunction, tfcBooleans = map evalRow (transpose $ map vcBooleans varColumns) }
  where
    varNames = map columnVar varColumns
    evalRow row = evaluate truthFunction (makeEvalMap varNames row)

getTFVars :: PLProp -> Maybe [String]
getTFVars T             = Nothing
getTFVars F             = Nothing
getTFVars (P s)         = Just [s]
getTFVars (Not p)       = getTFVars p
getTFVars (And p q)     = getTFVars p <> getTFVars q
getTFVars (Or p q)      = getTFVars p <> getTFVars q
getTFVars (Xor p q)     = getTFVars p <> getTFVars q    
getTFVars (Implies p q) = getTFVars p <> getTFVars q 

evaluate :: PLProp -> EvalMap -> Bool
evaluate T _               = True
evaluate F _               = False
evaluate (P var) env       = maybe False id (lookup var env)
evaluate (Not p) env       = not (evaluate p env)
evaluate (And p q) env     = evaluate p env && evaluate q env
evaluate (Or p q) env      = evaluate p env || evaluate q env
evaluate (Xor p q) env     = evaluate p env /= evaluate q env
evaluate (Implies p q) env = not (evaluate p env) || evaluate q env

type EvalMap = [(String, Bool)]

makeEvalMap :: [String] -> [Bool] -> EvalMap
makeEvalMap vars values = zip vars values

makeTruthTable :: [String] -> [PLProp] -> TruthTable
makeTruthTable varNames truthFunctions =
  TruthTable { variableColumns = varCols, truthFunctionColumns = tfCols }
  where
    rows = replicateM (length varNames) [True, False]
    varCols = makeVColumns varNames rows
    tfCols = map (\tf -> makeTFColumn tf varCols) truthFunctions

