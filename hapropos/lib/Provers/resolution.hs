module Provers.Resolution where

import Prop
import Typeclasses.Resolution (ResolutionInput(..), ResolutionOutput(..), resolution)
import Control.Monad (forM)
import Text.Megaparsec (runParser, errorBundlePretty)
import Text.Megaparsec (Parsec)
import Parse.MegaParsec (parseProp)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S (fromList)

parseFile :: FilePath -> IO (Either String (ResolutionInput Prop))
parseFile path = do
    content <- TIO.readFile path
    let contentLines = T.lines content
    case contentLines of
        [] -> pure $ Left "Empty file"
        (x:xs) -> do
            let goalResult = runParser parseProp "goal" x
            case goalResult of
                Left err -> pure $ Left $ "Error parsing goal: " ++ errorBundlePretty err
                Right goal -> do
                    let axiomsResult = traverse (runParser parseProp "axiom") xs
                    case axiomsResult of
                        Left err -> pure $ Left $ "Error parsing axioms: " ++ errorBundlePretty err
                        Right axioms -> pure $ Right (ResolutionInput {resolutionGoal = goal, resolutionAxioms = axioms})

proveTheorem :: ResolutionInput Prop -> ResolutionOutput Prop
proveTheorem (ResolutionInput {resolutionGoal = g, resolutionAxioms = axs}) =
  resolution g (S.fromList axs)


main :: IO ()
main = do
    result <- parseFile "test/theorem1.txt"
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right ri@(ResolutionInput {resolutionGoal = goal, resolutionAxioms = axioms}) -> case proveTheorem ri of
            ResolutionOutput {resolutionProvable = False, resolutionProof = _ } -> putStrLn $ "Proof failed: not provable."
            ResolutionOutput {resolutionProvable = True, resolutionProof = Nothing} -> putStrLn $ "Theorem provable but no proof."
            ResolutionOutput {resolutionProvable = True, resolutionProof = Just p} -> putStrLn $ "Theorem provable with proof: " ++ (foldr (<>) "" $ map show p)
