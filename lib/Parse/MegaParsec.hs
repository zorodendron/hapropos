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
propParser :: Parser Prop
propParser = choice
  [ trueParser           -- Parses "True"
  , falseParser          -- Parses "False"
  --, varParser            -- Parses expressions like "Var x"
  , quotedVarParser      -- Parses expressions like "Var "x""
  , notParser            -- Parses expressions like "Not P"
  , andParser            -- Parses expressions like "And P Q"
  , orParser             -- Parses expressions like "Or P Q"
  , xorParser            -- Parses expressions like "Xor P Q" 
  , impliesParser
  , parens propParser    -- Parses expressions like "(And P Q)"
  ]

-- Variant parsers.

trueParser :: Parser Prop
trueParser = T <$ symbol "T"

falseParser :: Parser Prop
falseParser = F <$ symbol "F"

varParser :: Parser Prop 
varParser = Var <$> (symbol "Var" *> some letterChar)

--quotedVarParser :: Parser Prop
--quotedVarParser = Var <$> between (char '"') (char '"') (many (noneOf ['"']))

quotedVarParser = do
  _ <- symbol "Var"
  name <- between (char '"') (char '"') (many (noneOf ['"']))
  return $ Var name


qvp2 = do
  q1 <- char '"'
  p <- varParser 
  q2 <- char '"'
  return p 


andParser :: Parser Prop
andParser = do
  _ <- symbol "And"
  p1 <- propParser
  p2 <- propParser
  return $ And p1 p2

orParser :: Parser Prop
orParser = do
  _ <- symbol "Or"
  p1 <- propParser
  p2 <- propParser
  return $ And p1 p2

xorParser :: Parser Prop
xorParser = do
  _ <- symbol "Xor"
  p1 <- propParser
  p2 <- propParser
  return $ Xor p1 p2

impliesParser :: Parser Prop
impliesParser = do
  _ <- symbol "Implies"
  p1 <- propParser
  p2 <- propParser
  return $ Implies p1 p2

notParser :: Parser Prop
notParser = do
  _ <- symbol "Not"
  p <- propParser
  return $ Not p

quantParser :: Parser Prop
quantParser = do
  q <- quantifierParser
  prop <- propParser
  pred <- predicateParser
  return $ Quant q prop pred

quantifierParser :: Parser Quantifier
quantifierParser = (Exists <$ symbol "Exists") <|> (Forall <$ symbol "Forall")

predicateParser :: Parser String
predicateParser = some letterChar <* spaceConsumer

-- Parse thing in parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

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
  prop <- lexeme $ parens propParser
  return $ Bind name prop

bindParser :: Parser AST
bindParser = do
  _ <- symbol "Bind"
  name <- some letterChar
  _ <- space
  prop <- parens propParser
  return $ Bind name prop

pexpParser :: Parser AST
pexpParser = do
  _ <- symbol "Pexp"
  prop <- parens propParser
  return $ Pexp prop

astParser :: Parser AST
astParser = choice 
  [ try bindNameParser 
  , pexpParser -- What happen if all entry start w try
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


