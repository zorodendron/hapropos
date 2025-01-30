module Typeclasses.Resolution where

import Prop as P
import qualified Data.Set as S
import Typeclasses.NormalForms

data ResolutionError =
    NoncomplementaryLiterals
  | InvalidClauseStructure
  | CircularResolution
  | UnsatButNoEmptyClause
  | PrematureTermination
  | EmptyClauseFromTautologies
  | QuantifierError
  | UnsupportedOperator

data ResolutionInput prop = ResolutionInput
    { resolutionGoal :: prop
    , resolutionAxioms :: [prop]
    } deriving (Show)

data ResolutionOutput prop = ResolutionOutput
    { resolutionProvable :: Bool
    , resolutionProof :: Maybe [prop]
    } deriving (Show)

-- the resolution rule is applied between two clauses that contain complementary literals
-- (e.g., A in one clause and ¬A in the other). the rule combines the two clauses, removing the complementary literals, and forms a new clause

data Satisfiability = Sat | Unsat deriving (Show)
data Validity = Valid | Invalid deriving (Show)

class Resolution prop where
  resolution :: prop -> S.Set prop -> ResolutionOutput prop
  toClauses :: prop -> Clause prop
  toLiterals :: prop -> Clause prop
  complimentaryClause :: prop -> prop
  toCNF :: prop -> CNF prop

type Literal = P.Prop -- a literal is a proposition that is either an atom or the negation of an atom
type Clause a = S.Set a -- a clause is a disjunction of literals
type CNF a    = S.Set (Clause a) -- a conjunction of clauses. the set is in conjunctive normal form

-- first convert p into cnf as a Prop (like you already do).
-- then parse that ast into a set-of-sets representation.


-- 'litSet' converts a disjunction into a set of 'literals.'
litSet :: P.Prop -> S.Set P.Prop
litSet (P.Or a b) = S.union (litSet a) (litSet b)
litSet x          = S.singleton x  -- presumably x is a literal

resolveClauses :: Clause Prop -> Clause Prop -> S.Set (Clause Prop)
resolveClauses c1 c2 =
  S.fromList
  [ (S.delete l c1') `S.union` (S.delete (negateLiteral l) c2')
  | l <- S.toList c1'
  , (negateLiteral l) `S.member` c2
  ]
  where c1' = c1; c2' = c2

resolutionLoop :: CNF Prop -> CNF Prop
resolutionLoop clauses =
  let newClauses = S.unions [resolveClauses c1 c2 | c1 <- S.toList clauses, c2 <- S.toList clauses, c1 /= c2]
      combined   = clauses `S.union` newClauses
  in if combined == clauses
     then clauses
     else resolutionLoop combined



literalsAreComplementary :: P.Prop -> P.Prop -> Bool
literalsAreComplementary p q = p == Not q || q == Not p

hasComplementIn :: Literal -> Literal -> Bool
hasComplementIn = literalsAreComplementary

negateLiteral :: Literal -> Literal
negateLiteral (P.Not p) = p
negateLiteral p = P.Not p


instance Resolution P.Prop where
  resolution goal knowledgeBase =
    let cnfConjunction :: CNF P.Prop
        cnfConjunction = toCNF $ toConjunctiveNormalForm (foldr P.And (P.Not goal) knowledgeBase) -- creates the conjunction of the NEGATION OF THE GOAL and the knowledge base; converts the conjunction to CNF
        finalClauses = resolutionLoop cnfConjunction
          in if S.member S.empty finalClauses
          then ResolutionOutput {resolutionProvable = True, resolutionProof = Just []}
          else ResolutionOutput {resolutionProvable = False, resolutionProof = Nothing}


-- the knowledge base is a set of propositions which can contain general propositions
-- but also propositions that are relevant to this specific domain
-- this means i can make the first domain specific theorem prover for philosophy!
-- ideas for domains:
-- epistemology or epistemic logic (fitch's paradox, but may need modal logic)

  toCNF :: P.Prop -> CNF P.Prop
  toCNF p =
    let pCNFAst = distributeOr (toNegationNormalForm (eliminateImplications p))
        propToCNF (P.And a b) = S.union (propToCNF a) (propToCNF b)
        propToCNF x           = S.singleton (litSet x)
    in propToCNF pCNFAst

-- probably need to fully reimplement
  toClauses :: P.Prop -> Clause P.Prop
  toClauses (P.And p q) = S.union (toClauses p) (toClauses q)
  toClauses clause = toLiterals clause  -- used to be S.singleton $ toLiterals clause but that gave me a type error

  toLiterals :: P.Prop -> Clause P.Prop
  toLiterals (P.Or p q) = S.union (toLiterals p) (toLiterals q)
  toLiterals literal = S.singleton literal
