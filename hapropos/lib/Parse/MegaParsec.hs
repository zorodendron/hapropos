{-# LANGUAGE OverloadedStrings #-}

module Parse.MegaParsec where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void
import Data.Text (Text, lines, pack)
import Control.Monad (forM_)
import Control.Monad.Combinators.Expr

import Prop
import AST

-- Type of parser. No custom error type yet; so we use Void.
type Parser = Parsec Void Text

-- Main parser for the Prop data type.
parseProp :: Parser Prop
parseProp =
      parseTer   -- a Term can be a Prop
  <|> parseNot
  <|> parseAnd
  <|> parseOr
  <|> parseImplies
  <|> parseQuant
  <|> parseParens parseProp

--parseTermProp :: Parser Prop
--parseTermProp = Term <$> parseTerm -- promote a Term to a Prop

parseNot :: Parser Prop
parseNot = do
  string "Not"
  space
  p <- parseProp
  return $ Not p

parseAnd :: Parser Prop
parseAnd = do
  char '('
  p1 <- parseProp
  string "&&"
  p2 <- parseProp
  char ')'
  return $ And p1 p2

parseOr :: Parser Prop
parseOr = do
  char '('
  p1 <- parseProp
  string "||"
  p2 <- parseProp
  char ')'
  return $ Or p1 p2

parseImplies :: Parser Prop
parseImplies = do
  i <- string "Implies"
  space
  char '('
  p1 <- parseProp
  char ')'
  space
  char '('
  p2 <- parseProp
  char ')'
  return $ Implies p1 p2

  -- parse Quant
parseQuant :: Parser Prop
parseQuant = do
  string "Quant" -- matches "Quant"
  space
  quantifier <- parseQuantifier
  space
  var <- parseQuotedString -- parse the string variable
  space
  p <- parseProp -- parse the inner proposition
  return $ Quant quantifier var p

-- parse the quantifier (Forall or Exists)
parseQuantifier :: Parser Quantifier
parseQuantifier =
      (string "Forall" >> return Forall)
  <|> (string "Exists" >> return Exists)

-- helper to parse quoted strings
parseQuotedString :: Parser String
parseQuotedString = char '"' >> manyTill L.charLiteral (char '"')

parseTerm :: Parser Term
parseTerm =
      parseConst
  <|> parseVar
  <|> parseFunc

parseConst :: Parser Term
parseConst = do
  string "Const"
  space
  name <- parseQuotedString
  return $ Const name

parseVar :: Parser Term
parseVar = do
  string "Var"
  space
  name <- parseQuotedString
  return $ Var name

parseFunc :: Parser Term
parseFunc = do
  funcName <- parseQuotedString
  s1 <- space
  str <- parseQuotedString
  s2 <- space
  leftSquareBracket <- char '['
  args <- parseParens (parseTerm `sepBy` char ',')
  rightSquareBracket <- char ']'
  return $ Func funcName args -- e.g., "f(x, y)"

parseTruthValue :: Parser Prop
parseTruthValue =
      (string "True" >> return (TruthValue True))
  <|> (string "False" >> return (TruthValue False))

parseTer :: Parser Prop
parseTer = do
  string "Ter"
  space
  char '('
  term <- parseTerm -- delegate parsing the inner term
  char ')'
  return $ Ter term


-- Parse thing in parentheses.
parseParens :: Parser a -> Parser a
parseParens = between (symbol "(") (symbol ")")

-- Parse a symbol (lexeme).
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- Consumer that ignores spaces.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

-- Parser for AST begins here.

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

bindNameParser :: Parser AST
bindNameParser = do
  _ <- symbol "Bind"
  _ <- char '"'
  name <- some letterChar
  _ <- char '"'
  _ <- space
  prop <- lexeme $ parseParens parseProp
  return $ Bind name prop

bindParser :: Parser AST
bindParser = do
  _ <- symbol "Bind"
  name <- some letterChar
  _ <- space
  prop <- parseParens parseProp
  return $ Bind name prop

pexpParser :: Parser AST
pexpParser = do
  _ <- symbol "Pexp"
  prop <- parseParens parseProp
  return $ Pexp prop

astParser :: Parser AST
astParser = choice
  [ try bindNameParser
  , pexpParser
  ]

-- Parse single line.
parseAstLine :: Text -> IO ()
parseAstLine line = case parse astParser "" line of
  Left bundle -> putStr (errorBundlePretty bundle)  -- Print parse errors.
  Right ast -> print ast                            -- Print successfully parsed AST.

-- Read file and parse lines.
parseAstFile :: FilePath -> IO ()
parseAstFile filePath = do
  content <- readFile filePath
  let lines = Data.Text.lines (Data.Text.pack content)
  forM_ lines parseAstLine

testCases :: IO ()  -- Impromptu testing.
testCases = do
  parseAstFile "test/ASTData.txt"
