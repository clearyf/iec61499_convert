module ParseSt (parseSt, Statement(..), Symbol(..)) where

import BasePrelude hiding (try)
import Data.Functor.Identity (Identity)
import Text.Megaparsec
       (ParseError, ParsecT, char, digitChar, endBy, letterChar, parse,
        space, spaceChar, string, try)
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L

data Statement =
    Assignment String
               [Symbol]
    deriving (Show,Eq)

data Symbol
    = StBool Bool
    | StVar String
    | StInt Integer
    deriving (Show,Eq)

parseSt :: String -> Either ParseError [Statement]
parseSt = parse stParser "stdin"

stParser :: Parser [Statement]
stParser = statements

statements :: Parser [Statement]
statements = space *> statement `endBy` semicolon

semicolon :: Parser Char
semicolon = lexeme (char ';')

statement :: Parser Statement
statement = assignment

assignment :: Parser Statement
assignment = Assignment <$> lexIdentifier <* assignmentOp <*> expression

assignmentOp :: Parser String
assignmentOp = lexeme (string ":=")

expression :: Parser [Symbol]
expression = lexeme (some identifier)

identifier :: Parser Symbol
identifier = try lexTrue <|> try lexFalse <|> number <|> variable

lexNatNumber :: Parser Integer
lexNatNumber = lexeme L.integer

lexNumber :: Parser Integer
lexNumber = L.signed spaceConsumer lexNatNumber

number :: Parser Symbol
number = StInt <$> lexNumber

lexIdentifier :: Parser String
lexIdentifier =
  lexeme ((:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_'))

variable :: Parser Symbol
variable = StVar <$> lexIdentifier

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        (spaceChar *> pure ())
        (L.skipLineComment "//")
        (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexTrue :: Parser Symbol
lexTrue = symbol "TRUE" *> pure (StBool True)

lexFalse :: Parser Symbol
lexFalse = symbol "FALSE" *> pure (StBool False)

lexeme :: ParsecT String Identity a -> ParsecT String Identity a
lexeme = L.lexeme spaceConsumer
