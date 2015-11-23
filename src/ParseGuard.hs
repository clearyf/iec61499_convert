module ParseGuard (Guard(..), GuardCondition(..), parseGuard) where

import           BasePrelude hiding (try)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Megaparsec
       (ParseError, between, char, choice, digitChar, letterChar, parse,
        space, try)
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String (Parser)

data Guard = Guard
    { guardEvent :: Maybe String
    , guardCondition :: Maybe GuardCondition
    } deriving (Show,Eq)

data GuardCondition
    = GuardSubCondition [GuardCondition]
    | GuardVariable String
    | GuardAnd
    | GuardOr
    | GuardNot
    deriving (Show,Eq)

parseGuard :: Set String -> String -> Either ParseError Guard
parseGuard events = parse (doParseGuard events) "stdin"

doParseGuard :: Set String -> Parser Guard
doParseGuard events =
    Guard <$> (try (parseEvent events) <|> pure empty) <*>
    (stripLeadingAnd <$> parseCondition)

stripLeadingAnd :: (Alternative f) => GuardCondition -> f GuardCondition
stripLeadingAnd (GuardSubCondition (GuardAnd:xs)) = pure (GuardSubCondition xs)
stripLeadingAnd (GuardSubCondition []) = empty
stripLeadingAnd x = pure x

parseEvent :: Applicative f => Set String -> Parser (f String)
parseEvent events = do
    word <- identifier
    guard (Set.member word events)
    pure (pure word)

parens ::Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseCondition :: Parser GuardCondition
parseCondition = GuardSubCondition <$> many parseElement

parseElement :: Parser GuardCondition
parseElement =
    choice
        [ parens parseCondition
        , try andSymbol
        , try orSymbol
        , try notSymbol
        , GuardVariable <$> identifier]

identifier :: Parser String
identifier =
    lexeme
        ((:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_'))

andSymbol :: Parser GuardCondition
andSymbol = (symbol "AND" <|> symbol "&") *> pure GuardAnd

orSymbol :: Parser GuardCondition
orSymbol = (symbol "OR" <|> symbol "|") *> pure GuardOr

notSymbol :: Parser GuardCondition
notSymbol = (symbol "NOT" <|> symbol "!") *> pure GuardNot

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space
