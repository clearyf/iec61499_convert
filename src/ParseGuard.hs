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
    | GuardApprox
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
    Guard <$> option mzero (try (parseEvent events))
          <*> fmap stripLeadingAnd parseCondition

stripLeadingAnd :: (MonadPlus m) => GuardCondition -> m GuardCondition
stripLeadingAnd (GuardSubCondition (GuardAnd:xs)) = pure (GuardSubCondition xs)
stripLeadingAnd (GuardSubCondition []) = mzero
stripLeadingAnd x = pure x

parseEvent :: Applicative f => Set String -> Parser (f String)
parseEvent events = do
    word <- identifier
    guard (word `elem` events)
    pure (pure word)

parens ::Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseCondition :: Parser GuardCondition
parseCondition = fmap GuardSubCondition (many parseElement)

parseElement :: Parser GuardCondition
parseElement =
    choice
        [ parens parseCondition
        , try approxSymbol
        , try equalsSymbol
        , try andSymbol
        , try orSymbol
        , try notSymbol
        , try trueSymbol
        , try falseSymbol
        , fmap GuardVariable identifier]

identifier :: Parser String
identifier =
    lexeme
        ((:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_'))

equalsSymbol :: Parser GuardCondition
equalsSymbol = symbol "=" *> pure GuardEquals

approxSymbol :: Parser GuardCondition
approxSymbol = symbol "<>" *> pure GuardApprox

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
