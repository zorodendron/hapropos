module PLProp where

import Data.List (transpose)
import Control.Monad (replicateM)

-- Propositional Logic

data PLProp =
      TruthValue Bool
    | Var String
    | Not PLProp
    | And PLProp PLProp
    | Or PLProp PLProp
    | Implies PLProp PLProp
    deriving (Show, Eq)

data VariantData = VT | VF | VP | VN | VA | VO | VI

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

-- Returns a list of all variables in a proposition.
getTFVars :: PLProp -> Maybe [String]
getTFVars (TruthValue t)  = Nothing
getTFVars (Var s)         = Just [s]
getTFVars (Not p)       = getTFVars p
getTFVars (And p q)     = getTFVars p <> getTFVars q
getTFVars (Or p q)      = getTFVars p <> getTFVars q
getTFVars (Implies p q) = getTFVars p <> getTFVars q

-- Evaluates a proposition given an environment with truth assignments.
evaluate :: PLProp -> EvalMap -> Bool
evaluate (TruthValue t) env  = t
evaluate (Var var) env     = maybe False id (lookup var env) -- returns False if the variable is not in the environment, True otherwise.
evaluate (Not p) env       = not (evaluate p env)
evaluate (And p q) env     = evaluate p env && evaluate q env
evaluate (Or p q) env      = evaluate p env || evaluate q env
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
