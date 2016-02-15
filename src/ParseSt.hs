module ParseSt
       (CaseSubExpression(..), IECVariable(..), LValue(..), StMonoOp(..),
        StBinaryOp(..), Statement(..), Value(..), Width(..), parseSt,
        parseValue, iECtypeFromString)
       where

import           BasePrelude hiding (Prefix, try)
import           Data.Functor.Identity (Identity)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Megaparsec
       (Message(..), ParseError(..), ParsecT, SourcePos(..), (<?>),
        alphaNumChar, anyChar, between, char, choice, eof, errorIsUnknown,
        label, letterChar, lookAhead, manyTill, messageString, noneOf,
        option, parse, satisfy, sepBy, sepBy1, someTill, spaceChar, string,
        try, notFollowedBy)
import           Text.Megaparsec.Expr (Operator(..), makeExprParser)
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String (Parser)

--------------------------------------------------------------------------------

data Statement
    = Assignment LValue
                 Value
    | Declaration String
                  IECVariable
    | If Value
         [Statement]
    | IfElse Value
             [Statement]
             [Statement]
    | For String
          Int
          Int
          (Maybe Int)
          [Statement] -- Start End Step
    | While Value
            [Statement]
    | Repeat [Statement]
             Value
    | Case Value
           [(NonEmpty CaseSubExpression, [Statement])]
           [Statement]
    | Break
    | Return
    deriving (Show,Eq)

data CaseSubExpression
    = CaseInt Int
    | CaseRange Int
                Int
    deriving (Show,Eq)

data Value
    = StBool Bool
    | StChar Char
    | StMonoOp StMonoOp
               Value
    | StBinaryOp StBinaryOp
                 Value
                 Value
    | StLValue LValue
    | StInt Integer
    | StFloat Double
    | StTime Integer
    | StFunc String
             [Value]
    | StSubValue Value
    deriving (Show,Eq)

data StMonoOp
    = StNegate
    | StNot
    deriving (Show,Eq)

data StBinaryOp
    = StAddition
    | StSubtract
    | StExp
    | StMultiply
    | StDivide
    | StEquals
    | StNotEquals
    | StLessThanEquals
    | StLessThan
    | StGreaterThanEquals
    | StGreaterThan
    | StMod
    | StBitwiseAnd
    | StAnd
    | StOr
    | StXor
    deriving (Show,Eq)

data LValue
    = SimpleLValue String
    | ArrayLValue String
                  Value
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
    | IECArray (NonEmpty Int)
               IECVariable
    | IECString Int
    deriving (Show,Eq)

parseSt :: String -> String -> Either String [Statement]
parseSt name str = myparse stParser name str

parseValue :: String -> Either String Value
parseValue str = myparse value "Value" str

myparse :: Parser b -> String -> String -> Either String b
myparse p name str = left showError (parse p name str)
  where
    showError err =
        let src = errorPos err
        in "While reading '" <> sourceName src <> "' found error at line " <>
           show (sourceLine src) <> " col " <> show (sourceColumn src) <> "\n" <>
           -- Now nicely show the exact error location with a caret
           -- under the problematic point in the input.
           (lines str) !! (sourceLine src - 1) <> "\n" <>
           (replicate (sourceColumn src - 1) ' ') <> "^\n" <>
           errorText err
    errorText err
      | errorIsUnknown err = "Unknown parser error!"
      | otherwise =
          let (expected,other) = partition isExpected (errorMessages err)
          in foldMap otherMessages other <> expectedMessages expected
    otherMessages (Unexpected msg) = "Unexpectedly found token: " <> msg <> "\n"
    otherMessages (Message msg) = "Error: " <> msg <> "\n"
    otherMessages _ = mempty
    expectedMessages msgs
      | null msgs = mempty
      | otherwise =
          "Expected " <> fold (intersperse " or " (fmap messageString msgs))
    isExpected (Expected _) = True
    isExpected _ = False

--------------------------------------------------------------------------------

-- The Lexer combinators only consume whitespace after tokens, so the
-- first step must be to consume all whitespace at the start of the
-- input string.
stParser :: Parser [Statement]
stParser =
    spaceConsumer *>
    (mappend <$> option mempty parseVarDecls <*> statementsTill eof)

--------------------------------------------------------------------------------

parseVarDecls :: Parser [Statement]
parseVarDecls =
    try (keyword "VAR") *> parseVarDecl `manyTill` try (keyword "END_VAR" *> semicolon)

parseVarDecl :: Parser Statement
parseVarDecl =
    Declaration <$> identifier <* symbol ":"
                <*> parseVarType <* semicolon
                <?> "variable declaration"

parseVarType :: Parser IECVariable
parseVarType =
    label "IEC Variable Type" $
    do name <- identifier
       indices <- optional (brackets parseIndices)
       createVariableType name indices
  where
    createVariableType name Nothing = vartypeFromString name Nothing
    createVariableType name x@(Just i) = do
        subtype <- vartypeFromString name x
        pure $! IECArray i subtype
    parseIndices = fmap NE.fromList (lexInt `sepBy1` comma)

iECtypeFromString :: String -> Either String IECVariable
iECtypeFromString str = myparse parseVarType "An IEC type definition" str

vartypeFromString :: String -> Maybe (NonEmpty Int) -> Parser IECVariable
vartypeFromString str indices = do
    case lookup lowerCased alist of
        Nothing -> fail ("Unhandled IEC variable type: " <> str)
        Just a -> a
  where
    lowerCased = fmap toLower str
    stringType =
        case indices of
            Just (size :| []) -> pure $! IECString size
            _ -> fail ("Invalid indices: " <> show indices <>
                       " for variable type: " <> str)
    alist =
        [ ("bool", pure IECBool)
        ,
          -- Real types
          ("real", pure IECReal)  -- TODO REAL is 32bit
        , ("lreal", pure IECReal) -- TODO LREAL is 0..1 64 bit
        ,
          -- Unsigned integer types
          ("byte", pure (IECUInt Eight))
        , ("usint", pure (IECUInt Eight))
        , ("word", pure (IECUInt Sixteen))
        , ("uint", pure (IECUInt Sixteen))
        , ("udint", pure (IECUInt ThirtyTwo))
        , ("dword", pure (IECUInt ThirtyTwo))
        , ("ulint", pure (IECUInt SixtyFour))
        ,
          -- Signed integer types
          ("sint", pure (IECInt Eight))
        , ("int", pure (IECInt Sixteen))
        , ("dint", pure (IECInt ThirtyTwo))
        , ("lint", pure (IECInt SixtyFour))
        ,
          -- Various other types
          ("time", pure IECTime)
        , ("date_and_time", fail "Can't handle DATE_AND_TIME type!")
        , ("time_of_day", fail "Can't handle TIME_OF_DAY type!")
        , ("string", stringType)]

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
    fmap pure (parseFor <|>
               parseCase <|>
               parseIf <|>
               parseWhile <|>
               parseReturn <|>
               parseBreak <|>
               parseRepeat <|>
               assignment)

parseIf :: Parser Statement
parseIf =
    createIf <$> parseIfToThen
             <*> optional (try parseFirstBranch)
             <*> parseLastBranch
             <?> "statement"
  where
    parseIfToThen = try (keyword "IF") *> value <* keyword "THEN"
    parseFirstBranch = statementsTill (try (keyword "ELSE"))
    parseLastBranch = statementsTill (try (keyword "END_IF"))
    -- If the first branch can't be parsed because there is no "ELSE"
    -- clause, then the "last" branch parsed is actually the true
    -- branch and not the false one.
    createIf a Nothing b = If a b
    createIf a (Just b) c = IfElse a b c

-- FOR count := 0 TO 10 BY 1 DO lll := count; END_FOR;
parseFor :: Parser Statement
parseFor =
  For <$> (try (keyword "FOR") *> identifier)
      <*> (assignmentOp *> lexInt)
      <*> (keyword "TO" *> lexInt)
      <*> optional (keyword "BY" *> lexInt)
      <*> (keyword "DO" *> statementsTill (try (keyword "END_FOR")))
      <?> "FOR loop"

parseWhile :: Parser Statement
parseWhile =
  While <$> (try (keyword "WHILE") *> value <* keyword "DO")
        <*> statementsTill (try (keyword "END_WHILE"))
        <?> "WHILE loop"

parseRepeat :: Parser Statement
parseRepeat =
  Repeat <$> (try (keyword "REPEAT") *> statementsTill (try (keyword "UNTIL")))
         <*> value <* keyword "END_REPEAT"
         <?> "REPEAT loop"

parseCase :: Parser Statement
parseCase =
  Case <$> (try (keyword "CASE") *> value <* keyword "OF")
       <*> parseCaseExpression `manyTill` endNormalCaseStatements
       <*> handleElse <* keyword "END_CASE"
       <?> "CASE statement"
  where endNormalCaseStatements =
          try (lookAhead (keyword "END_CASE")) <|> try (keyword "ELSE")
        handleElse =
          option mempty (statementsTill (try (lookAhead (keyword "END_CASE"))))

parseCaseExpression :: Parser (NonEmpty CaseSubExpression, [Statement])
parseCaseExpression =
  (,) <$> parseCaseSubExpression
      <*> statementsTill (try (lookAhead (keyword "END_CASE")) <|>
                          try (lookAhead (keyword "ELSE")) <|>
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
parseReturn = Return <$ try (keyword "RETURN")

parseBreak :: Parser Statement
parseBreak = Break <$ try (keyword "EXIT")

assignment :: Parser Statement
assignment = Assignment <$> lValue <* assignmentOp <*> value

--------------------------------------------------------------------------------

value :: Parser Value
value = makeExprParser terminals operatorTable <?> "value"

terminals :: Parser Value
terminals = parseSubValue <|>
            parseChar <|>
            number <|>
            lexBool <|>
            parseTime <|>
            fmap StLValue (try lValue) <|>
            parseFunction

operatorTable:: [[Operator Parser Value]]
operatorTable= [[prefix "-" (StMonoOp StNegate)
                ,prefix "NOT" (StMonoOp StNot)
                ,prefix "!" (StMonoOp StNot)]
               ,[binary "**" (StBinaryOp StExp)]
               ,[binary "*" (StBinaryOp StMultiply)
                ,binary "/" (StBinaryOp StDivide)
                ,binary "MOD" (StBinaryOp StMod)]
               ,[binary "+" (StBinaryOp StAddition)
                ,binary "-" (StBinaryOp StSubtract)]
               ,[binary "<=" (StBinaryOp StLessThanEquals)
                 -- This op is special as it’s only valid if the
                 -- following char isn’t ’>’.
                ,lessThanOp
                ,binary ">=" (StBinaryOp StGreaterThanEquals)
                ,binary ">" (StBinaryOp StGreaterThan)]
               ,[binary "=" (StBinaryOp StEquals)
                ,binary "<>" (StBinaryOp StNotEquals)]
               ,[binary "AND" (StBinaryOp StAnd)
                ,binary "&" (StBinaryOp StAnd)]
               ,[binary "XOR" (StBinaryOp StXor)]
               ,[binary "OR" (StBinaryOp StOr)
                ,binary "|" (StBinaryOp StOr)]]

parseChar :: Parser Value
parseChar = StChar <$> (singleQuote *> anyChar) <* singleQuote
  where
    singleQuote = lexeme (char '\'')

parseFunction :: Parser Value
parseFunction = StFunc <$> identifier <*> parens parseArgs

parseArgs :: Parser [Value]
parseArgs = value `sepBy` comma

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parseSubValue :: Parser Value
parseSubValue = StSubValue <$> parens value

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

assignmentOp :: Parser String
assignmentOp = symbol ":="

keywords :: Set String
keywords = Set.fromList ["IF", "THEN", "ELSE", "END_IF", "FOR", "END_FOR"
                        ,"VAR", "END_VAR", "CASE", "END_CASE", "WHILE",
                         "END_WHILE", "REPEAT", "END_REPEAT", "EXIT", "RETURN"]

parseTime :: Parser Value
parseTime = lexeme (try (symbol "t#") *> (try parseShortTime <|> parseLongTime))

parseLongTime :: Parser Value
parseLongTime = StTime . sum <$> parseStuff `someTill` lookAhead notDigit
  where
    units = [("h", 60 * 60 * 1000), ("ms", 1), ("s", 1000), ("m", 60 * 1000)]
    parseUnits = choice (map (try . string . fst) units)
    -- fromJust is completely safe here, s must be one of
    -- the strings that was in units.
    calcTime n s = n * fromJust (lookup s units)
    parseStuff = calcTime <$> justAnInteger <*> parseUnits
    notDigit = satisfy (not . isDigit) <?> "non digit"

parseShortTime :: Parser Value
parseShortTime = StTime <$> justAnInteger <* lookAhead (noneOf "hms")

justAnInteger :: Parser Integer
justAnInteger = L.integer

lexBool :: Parser Value
lexBool = try (keyword "TRUE" $> StBool True <|> keyword "FALSE" $> StBool False)

lessThanOp :: Operator Parser Value
lessThanOp =
    InfixL
        (try
             (symbol "<" *> notFollowedBy (symbol ">") *>
              pure (StBinaryOp StLessThan)))

binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (reservedOp name *> pure f)

prefix :: String -> (a -> a) -> Operator Parser a
prefix name f = Prefix (reservedOp name *> pure f)

reservedOp :: String -> Parser String
reservedOp = try . symbol

lexInt :: Parser Int
lexInt = fmap fromIntegral lexInteger

lexInteger :: Parser Integer
lexInteger = L.signed spaceConsumer (lexeme L.integer)

lexNumber :: Parser (Either Integer Double)
lexNumber = lexeme L.number

number :: Parser Value
number = fmap (StInt ||| StFloat) lexNumber

identifier :: Parser String
identifier = do
    word <- lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))
    guard (word `notElem` keywords) <?> "non-keyword"
    pure word

lValue :: Parser LValue
lValue =
    f <$> identifier <*> optional (brackets value) <*
    notFollowedBy (symbol "(")
  where
    f name Nothing = SimpleLValue name
    f name (Just s) = ArrayLValue name s

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        (spaceChar *> pure ())
        (L.skipLineComment "//")
        (L.skipBlockComment "(*" "*)")

keyword :: String -> Parser String
keyword s = lexeme (string s <* notFollowedBy (alphaNumChar <|> char '_'))

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: ParsecT String Identity a -> ParsecT String Identity a
lexeme = L.lexeme spaceConsumer

semicolon :: Parser Char
semicolon = lexeme (char ';')

comma :: Parser Char
comma = lexeme (char ',')
