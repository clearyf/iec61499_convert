{-# LANGUAGE NoImplicitPrelude #-}
module ParseSt (parseSt, Statement(..)) where

import BasePrelude
import Text.Megaparsec
       (ParseError, char, digitChar, endBy, letterChar, parse, satisfy,
        string)
import Text.Megaparsec.String (Parser)

data Statement
  = Assignment String String
  deriving (Show,Eq)

parseSt :: String -> Either ParseError [Statement]
parseSt = parse stParser "stdin"

stParser :: Parser [Statement]
stParser = statements

statements :: Parser [Statement]
statements = statement `endBy` semicolon

semicolon :: Parser Char
semicolon = char ';'

statement :: Parser Statement
statement = assignment

assignment :: Parser Statement
assignment = do lhs <- identifier
                _ <- string ":="
                rhs <- expression
                return $! Assignment lhs rhs

expression :: Parser String
expression = many (satisfy (\c -> c /= ';'))

identifier :: Parser String
identifier =
  (:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_')
