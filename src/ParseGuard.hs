module ParseGuard (Guard(..), GuardCondition(..), parseGuard) where

import           BasePrelude hiding (try)
import           Data.Set (Set)
import           Text.Megaparsec
       (ParseError, between, char, choice, digitChar, letterChar, option,
        parse, space, try)
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String (Parser)

data Guard = Guard
    { guardEvent :: Maybe String
    , guardCondition :: Maybe GuardCondition
    } deriving (Show,Eq)

data GuardCondition
    = GuardSubCondition [GuardCondition]
    | GuardVariable String
    | GuardEquals
    | GuardAnd
    | GuardOr
    | GuardNot
    | GuardTrue
    | GuardFalse
    deriving (Show,Eq)

parseGuard :: Set String -> String -> Either ParseError Guard
parseGuard events str = parse (doParseGuard events) str str

doParseGuard :: Set String -> Parser Guard
doParseGuard events =
    Guard <$> option Nothing (try (parseEvent events))
          <*> (stripLeadingAnd <$> parseCondition)

stripLeadingAnd :: (Alternative f) => GuardCondition -> f GuardCondition
stripLeadingAnd (GuardSubCondition (GuardAnd:xs)) = pure (GuardSubCondition xs)
stripLeadingAnd (GuardSubCondition []) = empty
stripLeadingAnd x = pure x

parseEvent :: Applicative f => Set String -> Parser (f String)
parseEvent events = do
    word <- identifier
    guard (elem word events)
    pure (pure word)

parens ::Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseCondition :: Parser GuardCondition
parseCondition = GuardSubCondition <$> many parseElement

parseElement :: Parser GuardCondition
parseElement =
    choice
        [ parens parseCondition
        , try equalsSymbol
        , try andSymbol
        , try orSymbol
        , try notSymbol
        , try trueSymbol
        , try falseSymbol
        , GuardVariable <$> identifier]

identifier :: Parser String
identifier =
    lexeme
        ((:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_'))

equalsSymbol :: Parser GuardCondition
equalsSymbol = (symbol "=") *> pure GuardEquals

andSymbol :: Parser GuardCondition
andSymbol = (symbol "AND" <|> symbol "&") *> pure GuardAnd

orSymbol :: Parser GuardCondition
orSymbol = (symbol "OR" <|> symbol "|") *> pure GuardOr

notSymbol :: Parser GuardCondition
notSymbol = (symbol "NOT" <|> symbol "!") *> pure GuardNot

trueSymbol :: Parser GuardCondition
trueSymbol = (symbol "TRUE" <|> symbol "1") *> pure GuardTrue

falseSymbol :: Parser GuardCondition
falseSymbol = (symbol "FALSE" <|> symbol "0") *> pure GuardFalse

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space
