{-# LANGUAGE TupleSections #-}
module ParseSt
       (CaseSubExpression(..), IECVariable(..), LValue(..), Statement(..),
        Value(..), Width(..), parseSt, iECtypeFromString)
       where

import           BasePrelude hiding (Prefix, try)
import           Data.Functor.Identity (Identity)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Megaparsec
       (ParseError, ParsecT, (<?>), between, char, choice, digitChar, eof,
        letterChar, lookAhead, manyTill, noneOf, option, parse, satisfy,
        sepBy, sepBy1, someTill, spaceChar, string, try)
import           Text.Megaparsec.Expr (Operator(..), makeExprParser)
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String (Parser)

--------------------------------------------------------------------------------

data Statement
  = Assignment LValue Value
  | Declaration String IECVariable
  | If Value [Statement]
  | IfElse Value [Statement] [Statement]
  | For String Int Int (Maybe Int) [Statement] -- Start End Step
  | While Value [Statement]
  | Repeat [Statement] Value
  | Case Value [(NonEmpty CaseSubExpression, [Statement])] [Statement]
  | Break
  | Return
  deriving (Show,Eq)

data CaseSubExpression
  = CaseInt Int
  | CaseRange Int Int
  deriving (Show,Eq)

data Value
  = StBool Bool
  | StAddition Value Value
  | StSubtract Value Value
  | StNegate Value
  | StExp Value Value
  | StMultiply Value Value
  | StDivide Value Value
  | StEquals Value Value
  | StNotEquals Value Value
  | StLessThanEquals Value Value
  | StLessThan Value Value
  | StGreaterThanEquals Value Value
  | StGreaterThan Value Value
  | StNot Value
  | StMod Value Value
  | StBitwiseAnd Value Value
  | StAnd Value Value
  | StOr Value Value
  | StXor Value Value
  | StLValue LValue
  | StInt Integer
  | StFloat Double
  | StTime Integer
  | StFunc String [Value]
  | StSubValue Value
  deriving (Show,Eq)

data LValue
  = SimpleLValue String
  | ArrayLValue String Value
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

--------------------------------------------------------------------------------

-- The Lexer combinators only consume whitespace after tokens, so the
-- first step must be to consume all whitespace at the start of the
-- input string.
stParser :: Parser [Statement]
stParser =
    spaceConsumer *>
    ((<>) <$> option mempty (try parseVarDecls)
          <*> statementsTill eof)

--------------------------------------------------------------------------------

parseVarDecls :: Parser [Statement]
parseVarDecls =
    symbol "VAR" *> parseVarDecl `manyTill` try (symbol "END_VAR" *> semicolon)

parseVarDecl :: Parser Statement
parseVarDecl =
    Declaration <$> lexeme lexIdentifier <* symbol ":"
                <*> parseVarType <* semicolon
                <?> "variable declaration"

parseVarType :: Parser IECVariable
parseVarType = f <$> lexeme lexIdentifier
                 <*> optional (brackets parseIndices)
                 <?> "variable type"
  where
    parseIndices = fmap NE.fromList (lexInt `sepBy1` comma)
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

--------------------------------------------------------------------------------

statementsTill :: Parser end -> Parser [Statement]
statementsTill end = fmap catMaybes (manyTill (statement <* semicolon) end)

-- Empty statements (bare semicolons) are always possible, so return
-- Nothing for them.  Use lookAhead to check for the colon, as if it's
-- there then it must be consumed in statementsTill, if statement
-- returns Nothing.
statement :: MonadPlus m => Parser (m Statement)
statement =
    (lookAhead semicolon *> pure mzero) <|>
    fmap pure (try parseFor <|>
               try parseCase <|>
               try parseIf <|>
               try parseWhile <|>
               try parseReturn <|>
               try parseBreak <|>
               try parseRepeat <|>
               assignment)

parseIf :: Parser Statement
parseIf =
    createIf <$> parseIfToThen
             <*> optional (try parseFirstBranch)
             <*> parseLastBranch
             <?> "statement"
  where
    parseIfToThen = symbol "IF" *> value <* symbol "THEN"
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
      <*> optional (symbol "BY" *> lexInt)
      <*> (symbol "DO" *> statementsTill (symbol "END_FOR"))
      <?> "FOR loop"

parseWhile :: Parser Statement
parseWhile =
  While <$> (symbol "WHILE" *> value <* symbol "DO")
        <*> (statementsTill (symbol "END_WHILE"))
        <?> "WHILE loop"

parseRepeat :: Parser Statement
parseRepeat =
  Repeat <$> (symbol "REPEAT" *> statementsTill (symbol "UNTIL"))
         <*> (value <* symbol "END_REPEAT")
         <?> "REPEAT loop"

parseCase :: Parser Statement
parseCase =
  Case <$> (symbol "CASE" *> value <* symbol "OF")
       <*> parseCaseExpression `manyTill` endNormalCaseStatements
       <*> handleElse <* symbol "END_CASE"
       <?> "CASE statement"
  where endNormalCaseStatements =
          try (lookAhead (symbol "END_CASE")) <|> try (symbol "ELSE")
        handleElse =
          option mempty (statementsTill (try (lookAhead (symbol "END_CASE"))))

parseCaseExpression :: Parser (NonEmpty CaseSubExpression, [Statement])
parseCaseExpression =
  (,) <$> parseCaseSubExpression
      <*> statementsTill (try (lookAhead (symbol "END_CASE")) <|>
                          try (lookAhead (symbol "ELSE")) <|>
                          try (lookAhead lexInt *> pure []))
      <?> "CASE sub-statements"

parseCaseSubExpression :: Parser (NonEmpty CaseSubExpression)
parseCaseSubExpression =
  fmap NE.fromList ((parseCaseInt `sepBy1` symbol ",") <* symbol ":")

parseCaseInt :: Parser CaseSubExpression
parseCaseInt =
  try (CaseRange <$> lexInt <*> (symbol "-" *> lexInt)) <|>
  (CaseInt <$> lexInt)

parseReturn :: Parser Statement
parseReturn = const Return <$> symbol "RETURN"

parseBreak :: Parser Statement
parseBreak = const Break <$> symbol "EXIT"

assignment :: Parser Statement
assignment = Assignment <$> lValue <* assignmentOp <*> value

--------------------------------------------------------------------------------

value :: Parser Value
value = makeExprParser terminals operatorTable <?> "value"

terminals :: Parser Value
terminals = parseSubValue <|>
            try parseFunction <|>
            number <|>
            try lexBool <|>
            parseTime <|>
            fmap StLValue lValue

operatorTable:: [[Operator Parser Value]]
operatorTable= [[prefix "-" StNegate
                ,prefix "NOT" StNot]
               ,[binary "**" StExp]
               ,[binary "*" StMultiply
                ,binary "/" StDivide
                ,binary "MOD" StMod]
               ,[binary "+" StAddition
                ,binary "-" StSubtract]
               ,[binary "<=" StLessThanEquals
                ,binary "<" StLessThan
                ,binary ">=" StGreaterThanEquals
                ,binary ">" StGreaterThan]
               ,[binary "=" StEquals
                ,binary "<>" StNotEquals]
               ,[binary "AND" StAnd
                ,binary "&" StAnd]
               ,[binary "XOR" StXor]
               ,[binary "OR" StOr]]

parseFunction :: Parser Value
parseFunction = StFunc <$> lexIdentifier <*> parens parseArgs

parseArgs :: Parser [Value]
parseArgs = value `sepBy` comma

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parseSubValue :: Parser Value
parseSubValue = StSubValue <$> parens value

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

assignmentOp :: Parser String
assignmentOp = lexeme (string ":=")

keywords :: Set String
keywords = Set.fromList ["IF", "THEN", "ELSE", "END_IF", "FOR", "END_FOR"
                        ,"VAR", "END_VAR", "CASE", "END_CASE", "WHILE",
                         "END_WHILE", "REPEAT", "END_REPEAT", "EXIT", "RETURN"]

parseTime :: Parser Value
parseTime = lexeme (symbol "t#" *> (try parseShortTime <|> parseLongTime))

parseLongTime :: Parser Value
parseLongTime = StTime . sum <$> parseStuff `someTill` lookAhead notDigit
  where
    units = [("h", (60 * 60 * 1000)),("ms", 1),("s", 1000),("m", 60 * 1000)]
    parseUnits = choice (fmap (try . string . fst) units)
                 -- fromJust is completely safe here, s must be one of
                 -- the strings that was in units.
    calcTime n s = n * (fromJust (lookup s units))
    parseStuff = calcTime <$> justAnInteger <*> parseUnits
    notDigit = satisfy (not . isDigit) <?> "non digit"

parseShortTime :: Parser Value
parseShortTime = StTime <$> justAnInteger <* lookAhead (noneOf "hms")

justAnInteger :: Parser Integer
justAnInteger = L.integer

lexBool :: Parser Value
lexBool = symbol "TRUE" $> StBool True <|>
          symbol "FALSE" $> StBool False

binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (reservedOp name >> return f)

prefix :: String -> (a -> a) -> Operator Parser a
prefix name f = Prefix (reservedOp name >> return f)

reservedOp :: String -> Parser String
reservedOp = try . lexeme . string

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
    guard (word `notElem` keywords) <?> "non-keyword"
    pure word

lValue :: Parser LValue
lValue = f <$> lexIdentifier <*> optional (brackets value)
  where
    f name Nothing = SimpleLValue name
    f name (Just s) = ArrayLValue name s

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        (spaceChar *> pure ())
        (L.skipLineComment "//")
        (L.skipBlockComment "(*" "*)")

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: ParsecT String Identity a -> ParsecT String Identity a
lexeme = L.lexeme spaceConsumer

semicolon :: Parser Char
semicolon = lexeme (char ';')

comma :: Parser Char
comma = lexeme (char ',')
