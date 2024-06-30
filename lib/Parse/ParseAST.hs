{-# LANGUAGE OverloadedStrings #-}

module Parse.ParseAST where

-- Old parser for informative purposes.
-- I moved on to use megaparsec.

import Control.Applicative
import Text.Trifecta
import Prop
import AST

-- Parser for the boolean constants T and F
constParser :: Parser Prop
constParser = (T <$ symbol "T") <|> (F <$ symbol "F")

-- Parser for the P constructor.
pParser :: Parser Prop
pParser = P <$> (symbol "P" *> brackets (some letter))

-- Parser for the Var constructor.
varParser :: Parser Prop
varParser = Var <$> (symbol "Var" *> some letter)

-- parser for the Not constructor.
notParser :: Parser Prop
notParser = Not <$> (symbol "Not" *> propParser)

-- combine all Prop parsers. 
propParser :: Parser Prop
propParser = choice [constParser, pParser, varParser, notParser, andParser, orParser, xorParser, impliesParser]

-- parser for the And constructor.
andParser :: Parser Prop
andParser = And <$> (symbol "And" *> parens propParser) <*> parens propParser

-- parser for the Or constructor.
orParser :: Parser Prop
orParser = Or <$> (symbol "Or" *> parens propParser) <*> parens propParser

-- parser for the Xor constructor.
xorParser :: Parser Prop
xorParser = Xor <$> (symbol "Xor" *> parens propParser) <*> parens propParser

-- parser for the Implies constructor.
impliesParser :: Parser Prop
impliesParser = Implies <$> (symbol "Implies" *> parens propParser) <*> parens propParser

-- parser for quantifiers.
quantifierParser :: Parser Quantifier
quantifierParser = (Exists <$ symbol "Exists") <|> (Forall <$ symbol "Forall")

parseConst :: String -> Result Prop
parseConst input = parseString constParser mempty input


-- Here begin the parsers for the AST.

bindParser :: Parser AST
bindParser = do 
  _ <- symbol "Bind" -- b probably needs to be in upper case.
  name <- nameParser
  prop <- propParser
  return $ Bind name prop

pexpParser :: Parser AST
pexpParser = do
  _ <- symbol "Pexp"
  prop <- propParser
  return $ Pexp prop

nameParser :: Parser String
nameParser = do
  firstChar <- letter
  restChars <- many alphaNum
  return (firstChar : restChars)
 
astParser :: Parser AST
astParser = bindParser <|> pexpParser

parseAST :: String -> Result AST
parseAST input = parseString astParser mempty input

-- Line and file parsing

astLinesParser :: Parser [AST]
astLinesParser = some (astParser <* optional newline) <* eof

parseASTFile :: FilePath -> IO (Maybe [AST])
parseASTFile filepath = parseFromFile astLinesParser filepath


-- Tests
parseT = case parseConst "T" of
  Success prop -> print prop
  Failure errInfo -> print (_errDoc errInfo)

astP :: String -> IO ()
astP astCode = case parseAST astCode of
  Success ast -> print ast
  Failure errInfo -> print (_errDoc errInfo) 

testCases :: IO ()
testCases = do 
  result <- parseASTFile "test/ASTData.txt"
  case result of 
    Just astList -> mapM_ print astList
    Nothing -> print "Error."
