{-# LANGUAGE TupleSections #-}
module ParseSt (parseSt, Statement(..), Symbol(..)) where

import           BasePrelude hiding (try)
import           Data.Functor.Identity (Identity)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Megaparsec
       (ParseError, ParsecT, char, choice, digitChar, eof, letterChar,
        lookAhead, manyTill, option, parse, someTill, space, spaceChar,
        string, try)
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String (Parser)

data Statement
    = Assignment String
                 [Symbol]
    | If [Symbol] [Statement]
    | IfElse [Symbol] [Statement] [Statement]
    deriving (Show,Eq)

data Symbol
    = StBool Bool
    | StOp String
    | StVar String
    | StInt Integer
    deriving (Show,Eq)

parseSt :: String -> Either ParseError [Statement]
parseSt = parse stParser "stdin"

stParser :: Parser [Statement]
stParser = space *> statementsTill eof

statementsTill :: Parser end -> Parser [Statement]
statementsTill end = catMaybes <$> manyTill (statement <* semicolon) end

semicolon :: Parser Char
semicolon = lexeme (char ';')

-- Empty statements (bare semicolons) are always possible, so return
-- Nothing for them.  Use lookAhead to check for the colon, as if it's
-- there then it must be consumed in statementsTill, if statement
-- returns Nothing.
statement :: Parser (Maybe Statement)
statement =
    (lookAhead semicolon *> pure Nothing) <|>
    (Just <$> (parseIf <|> assignment))

parseIf :: Parser Statement
parseIf =
    createIf <$> parseIfToThen
             <*> option Nothing (Just <$> try parseFirstBranch)
             <*> parseLastBranch
  where
    parseIfToThen = symbol "IF" *> (lexeme identifier `someTill` try (symbol "THEN"))
    parseFirstBranch = statementsTill (symbol "ELSE")
    parseLastBranch = statementsTill (symbol "END_IF")
    -- If the first branch can't be parsed because there is no "ELSE"
    -- clause, then the "last" branch parsed is actually the true
    -- branch and not the false one.
    createIf a Nothing b = If a b
    createIf a (Just b) c = IfElse a b c

assignment :: Parser Statement
assignment = Assignment <$> lexIdentifier <* assignmentOp <*> expression

assignmentOp :: Parser String
assignmentOp = lexeme (string ":=")

expression :: Parser [Symbol]
expression = lexeme (some identifier)

identifier :: Parser Symbol
identifier = number <|> operator <|> try lexTrue <|> try lexFalse <|> variable

keywords :: Set String
keywords = Set.fromList ["IF", "THEN", "ELSE", "END_IF"]

theSymbol :: String -> a -> Parser a
theSymbol sym result = symbol sym *> pure result

lexTrue :: Parser Symbol
lexTrue = theSymbol "TRUE" (StBool True)

lexFalse :: Parser Symbol
lexFalse = theSymbol "FALSE" (StBool False)

operator :: Parser Symbol
operator =
    let f x = theSymbol x (StOp x)
    in choice (fmap f ["+", "-", "*", "/", "=", "<", "<=", ">", ">="])

lexNatNumber :: Parser Integer
lexNatNumber = lexeme L.integer

lexNumber :: Parser Integer
lexNumber = L.signed spaceConsumer lexNatNumber

number :: Parser Symbol
number = StInt <$> lexNumber

lexIdentifier :: Parser String
lexIdentifier = do
  word <- lexeme ((:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_'))
  guard (word `notElem` keywords)
  pure word

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

lexeme :: ParsecT String Identity a -> ParsecT String Identity a
lexeme = L.lexeme spaceConsumer
