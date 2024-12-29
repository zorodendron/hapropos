module SmallProp where

import Data.List (nub, sort)
import Control.Monad (replicateM, forM_)

data SmallProp =
        T | F
          | P SmallProp
          | Not SmallProp
          | And SmallProp SmallProp
          | Or SmallProp SmallProp
          | Implies SmallProp SmallProp
  deriving Show

data VariantData = VT | VF | VP | VN | VA | VO | VI

-- Function to evaluate a SmallProp
eval :: [(String, Bool)] -> SmallProp -> Bool
eval _ T = True
eval _ F = False
eval vals (P p) = case lookup (show p) vals of
                    Just v -> v
                    Nothing -> error $ "Unbound proposition: " ++ show p
eval vals (Not p) = not (eval vals p)
eval vals (And p q) = eval vals p && eval vals q
eval vals (Or p q) = eval vals p || eval vals q
eval vals (Implies p q) = not (eval vals p) || eval vals q

-- Function to collect all atomic propositions in a SmallProp
collectProps :: SmallProp -> [String]
collectProps T = []
collectProps F = []
collectProps (P p) = [show p]
collectProps (Not p) = collectProps p
collectProps (And p q) = collectProps p ++ collectProps q
collectProps (Or p q) = collectProps p ++ collectProps q
collectProps (Implies p q) = collectProps p ++ collectProps q

-- Generate all possible truth assignments
truthAssignments :: [String] -> [[(String, Bool)]]
truthAssignments props = map (zip props) (replicateM (length props) [True, False])

-- Generate a truth table for a given SmallProp
truthTable :: SmallProp -> [([Bool], Bool)]
truthTable prop =
    let props = sort . nub $ collectProps prop
        assignments = truthAssignments props
    in map (\assignment -> (map snd assignment, eval assignment prop)) assignments

-- Print the truth table in a readable format
printTruthTable :: SmallProp -> IO ()
printTruthTable prop =
    let props = sort . nub $ collectProps prop
        table = truthTable prop
    in do
        putStrLn $ unwords (props ++ ["| Result"])
        putStrLn $ replicate (length props * 6 + 8) '-'
        mapM_ (\(vals, result) -> putStrLn $ unwords (map show vals ++ ["|", show result])) table

-- Here we are converting the smallpops to strings often but maybe we don't need to

-- todo: add more test cases

-- Example usage
testTT :: IO ()
testTT = do
    let propositions = [Implies (P T) (Or (P F) (Not (P T)))]

    forM_ propositions printTruthTable
