{-# LANGUAGE TupleSections #-}
module ParseSt
       (IECVariable(..), LValue(..), Statement(..), Value(..), Width(..),
        parseSt, iECtypeFromString)
       where

import           BasePrelude hiding (try)
import           Data.Functor.Identity (Identity)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Megaparsec
       (ParseError, ParsecT, between, char, choice, digitChar, eof,
        letterChar, lookAhead, manyTill, option, parse, sepBy, sepBy1,
        someTill, spaceChar, string, try)
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String (Parser)

data Statement
    = Assignment LValue (NonEmpty Value)
    | Declaration String IECVariable
    | If (NonEmpty Value) [Statement]
    | IfElse (NonEmpty Value) [Statement] [Statement]
    | For String Int Int Int [Statement] -- Start End Step
    deriving (Show,Eq)

data Value
    = StBool Bool
    | StOp String
    | StLValue LValue
    | StInt Integer
    | StFloat Double
    | StFunc String [NonEmpty Value]
    deriving (Show,Eq)

data LValue
    = SimpleLValue String
    | ArrayLValue String (NonEmpty Value)
    deriving (Show,Eq)

data Width
    = Eight
    | Sixteen
    | ThirtyTwo
    | SixtyFour
    deriving (Show,Eq)

data IECVariable
    = IECReal
    | IECInt Width
    | IECUInt Width
    | IECBool
    | IECTime
    | IECArray (NonEmpty Int) IECVariable
    deriving (Show,Eq)

parseSt :: String -> Either ParseError [Statement]
parseSt str = parse stParser str str

mappendA :: (Applicative m, Monoid a) => m a -> m a -> m a
mappendA = liftA2 (<>)

stParser :: Parser [Statement]
stParser =
    spaceConsumer *>
    option mempty (try parseVarDecls) `mappendA` statementsTill eof

statementsTill :: Parser end -> Parser [Statement]
statementsTill end = fmap catMaybes (manyTill (statement <* semicolon) end)

semicolon :: Parser Char
semicolon = lexeme (char ';')

comma :: Parser Char
comma = lexeme (char ',')

parseVarDecls :: Parser [Statement]
parseVarDecls =
    symbol "VAR" *> parseVarDecl `manyTill` try (symbol "END_VAR" *> semicolon)

parseVarDecl :: Parser Statement
parseVarDecl =
    Declaration <$> lexeme lexIdentifier <* symbol ":"
                <*> parseVarType <* semicolon

parseVarType :: Parser IECVariable
parseVarType = f <$> lexeme lexIdentifier
                 <*> optional (brackets parseIndices)
  where parseIndices = fmap NE.fromList (lexInt `sepBy1` comma)
        f name Nothing = vartypeFromString name
        f name (Just indices) = IECArray indices (vartypeFromString name)

iECtypeFromString :: String -> Either ParseError IECVariable
iECtypeFromString str = parse parseVarType str str

vartypeFromString :: String -> IECVariable
vartypeFromString str =
    fromMaybe (error "Unhandled IEC variable type!") (lookup lowerCased alist)
  where
    lowerCased = fmap toLower str
    alist =
        [ ("bool", IECBool)
        ,
          -- Real types
          ("real", IECReal)  -- TODO REAL is 32bit
        , ("lreal", IECReal) -- TODO LREAL is 0..1 64 bit
        ,
          -- Unsigned integer types
          ("byte", IECUInt Eight)
        , ("usint", IECUInt Eight)
        , ("word", IECUInt Sixteen)
        , ("uint", IECUInt Sixteen)
        , ("udint", IECUInt ThirtyTwo)
        , ("dword", IECUInt ThirtyTwo)
        , ("ulint", IECUInt SixtyFour)
        ,
          -- Signed integer types
          ("sint", IECInt Eight)
        , ("int", IECInt Sixteen)
        , ("dint", IECInt ThirtyTwo)
        , ("lint", IECInt SixtyFour)
        ,
          -- Various other types
          ("time", IECTime)
        , ("date_and_time", error "Can't handle DATE_AND_TIME type!")
        , ("time_of_day", error "Can't handle TIME_OF_DAY type!")]

-- Empty statements (bare semicolons) are always possible, so return
-- Nothing for them.  Use lookAhead to check for the colon, as if it's
-- there then it must be consumed in statementsTill, if statement
-- returns Nothing.
statement :: MonadPlus m => Parser (m Statement)
statement =
    (lookAhead semicolon *> pure mzero) <|>
    fmap pure (parseFor <|> parseIf <|> assignment)

parseIf :: Parser Statement
parseIf =
    createIf <$> fmap NE.fromList parseIfToThen
             <*> optional (try parseFirstBranch)
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

-- FOR count := 0 TO 10 BY 1 DO lll := count; END_FOR;
parseFor :: Parser Statement
parseFor =
    For <$> (symbol "FOR" *> lexIdentifier)
        <*> (assignmentOp *> lexInt)
        <*> (symbol "TO" *> lexInt)
        <*> (symbol "BY" *> lexInt)
        <*> (symbol "DO" *> statementsTill (symbol "END_FOR"))

parseFunction :: Parser Value
parseFunction = StFunc <$> lexIdentifier <*> parens parseArgs

parseArgs :: Parser [NonEmpty Value]
parseArgs = expression `sepBy` comma

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

assignment :: Parser Statement
assignment = Assignment <$> lValue <* assignmentOp <*> expression

assignmentOp :: Parser String
assignmentOp = lexeme (string ":=")

expression :: Parser (NonEmpty Value)
expression = fmap NE.fromList (lexeme (some identifier))

identifier :: Parser Value
identifier =
    choice
        [ operator
        , number
        , try lexTrue
        , try lexFalse
        , try parseFunction
        , fmap StLValue lValue]

keywords :: Set String
keywords = Set.fromList ["IF", "THEN", "ELSE", "END_IF", "FOR", "END_FOR"
                        ,"VAR", "END_VAR"]

theSymbol :: String -> a -> Parser a
theSymbol sym result = symbol sym *> pure result

lexTrue :: Parser Value
lexTrue = theSymbol "TRUE" (StBool True)

lexFalse :: Parser Value
lexFalse = theSymbol "FALSE" (StBool False)

operator :: Parser Value
operator =
    let f x = theSymbol x (StOp x)
    in choice (fmap f ["+", "-", "*", "/", "=", "<", "<=", ">", ">="])

lexInt :: Parser Int
lexInt = fmap fromIntegral lexInteger

lexInteger :: Parser Integer
lexInteger = L.signed spaceConsumer (lexeme L.integer)

lexNumber :: Parser (Either Integer Double)
lexNumber = lexeme L.number

number :: Parser Value
number = fmap (either StInt StFloat) lexNumber

lexIdentifier :: Parser String
lexIdentifier = do
    word <- lexeme ((:) <$> letterChar
                        <*> many (letterChar <|> digitChar <|> char '_'))
    guard (word `notElem` keywords)
    pure word

lValue :: Parser LValue
lValue = f <$> lexIdentifier <*> optional (brackets expression)
  where
    f name Nothing = SimpleLValue name
    f name (Just s) = ArrayLValue name s

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
