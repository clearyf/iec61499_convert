module FbToUppaal (fbToUppaalModel, anAlgorithm) where

import           BasePrelude
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Reader.Class (ask)
import           Control.Monad.State.Class (get, put)
import           Control.Monad.Trans.Except (Except, runExcept)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, withReaderT)
import           Control.Monad.Trans.State.Lazy (State, evalState)
import           Control.Monad.Trans.Writer.Lazy (WriterT, execWriterT)
import           Control.Monad.Writer.Class (tell)
import           Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           OutputUppaal
       (UppaalModel(..), AState(..), UppaalChan(..), UppaalVar(..),
        Location(..), StateId(..), Transition(..))
import           ParseGuard (Guard(..), parseGuard)
import           ParseIec61499
       (ECTransition(..), ECState(..), BasicFunctionBlock(..),
        FunctionBlockDescription(..), InterfaceList(..), Event(..),
        Variable(..), ECAction(..), BasicFunctionBlock(..),
        ECAlgorithm(..))
import           ParseSt
       (LValue(..), Statement(..), Value(..), IECVariable(..), Width(..),
        StMonoOp(..), StBinaryOp(..), CaseSubExpression(..))

-- | Converts IEC61499 BasicFunctionBlock to an UppaalModel
fbToUppaalModel :: BasicFunctionBlock -> Either String UppaalModel
fbToUppaalModel fb =
    UppaalModel <$> pure (fbName $ bfbDescription fb)
                <*> pure (inputChannels fb)
                <*> pure (outputChannels fb)
                <*> inputParameters fb
                <*> outputParameters fb
                <*> localParameters fb
                <*> pure (locations fb)
                <*> transitions fb
                <*> definitions
  where
    definitions =
        mappend <$> (right fold . traverse anAlgorithm . bfbAlgorithms $ fb)
                <*> (pure (createLibraryFunctions fb))

--------------------------------------------------------------------------------
-- Handle events

extractChannels :: String -> (InterfaceList -> [Event]) -> BasicFunctionBlock -> [UppaalChan]
extractChannels prefix f =
    map (UppaalChan . (prefix <>) . eventName) . f . bfbInterfaceList

inputChannelPrefix :: String
inputChannelPrefix = "ic_"

outputChannelPrefix :: String
outputChannelPrefix = "oc_"

inputChannels :: BasicFunctionBlock -> [UppaalChan]
inputChannels = extractChannels inputChannelPrefix eventInputs

outputChannels :: BasicFunctionBlock -> [UppaalChan]
outputChannels = extractChannels outputChannelPrefix eventOutputs

--------------------------------------------------------------------------------
-- Handle variables

showVarType :: IECVariable -> Either String (String,String)
showVarType v =
    case v of
        IECBool -> Right ("bool", mempty)
        IECUInt Eight -> Right $ uintWithRange 8
        IECUInt Sixteen -> Right $ uintWithRange 16
        IECUInt ThirtyTwo -> Right $ uintWithRange 32
        IECUInt SixtyFour -> Right $ uintWithRange 64
        IECInt Eight -> Right $ intWithRange 7
        IECInt Sixteen -> Right $ intWithRange 15
        IECInt ThirtyTwo -> Right $ intWithRange 31
        IECInt SixtyFour -> Right $ intWithRange 63
        IECArray idxs var ->
            fmap
                (\x ->
                      ( fst x
                      , "[" <> foldMap id (NE.intersperse "," (fmap show idxs)) <>
                        "]"))
                (showVarType var)
        IECString size ->
            Right (uppaalIntWithRange 0 127, "[" <> show size <> "]")
        t -> Left ("Uppaal doesn't support " <> show t <> " type!")

uppaalIntWithRange :: Integer -> Integer -> String
uppaalIntWithRange from to = "int[" <> show from <> "," <> show to <> "]"

intWithRange :: Integer -> (String, String)
intWithRange i = (uppaalIntWithRange ((-2) ^ i) (2 ^ i - 1), mempty)

uintWithRange :: Integer -> (String, String)
uintWithRange i = (uppaalIntWithRange 0 (2 ^ i), mempty)

inputParameters :: BasicFunctionBlock -> Either String [UppaalVar]
inputParameters = traverse createUppaalVar . inputVariables . bfbInterfaceList

outputParameters :: BasicFunctionBlock -> Either String [UppaalVar]
outputParameters = traverse createUppaalVar . outputVariables . bfbInterfaceList

localParameters :: BasicFunctionBlock -> Either String [UppaalVar]
localParameters = traverse createUppaalVar . bfbVariables

createUppaalVar :: Variable -> Either String UppaalVar
createUppaalVar var =
    UppaalVar <$> fmap fst thetype
              <*> fmap ((variableName var <>) . snd) thetype
  where
    thetype = showVarType (variableType var)

--------------------------------------------------------------------------------
-- 3. Handle Locations
--
-- IEC61499 associates the advancedTransitions with the destination
-- state, whereas Uppaal associates the advancedTransitions with the
-- transition.  As well as that, each transition in Uppaal can either
-- wait on a channel or send on a channel, so each action in IEC61499
-- must become an additional urgent state in Uppaal.

-- For each output event in an action a secondary location is
-- required.

locationStartPrefix :: ECState -> String
locationStartPrefix state = "__start_" <> ecStateName state

locationEventPrefix :: ECState -> ECAction -> String
locationEventPrefix state action =
    fold
        [ "__action_"
        , ecStateName state
        , "__"
        , ecActionOutput action
        , "_"
        , ecActionAlgorithm action
        , "_"]

locations :: BasicFunctionBlock -> [Location]
locations fb = doFold states
  where
    states = getStatesMap (getBasicStates fb)
    doFold (StateMap m) = foldMap f m
    f (u,n) = map UrgentLocation u <> [Location n]

newtype StateMap =
    StateMap (Map String ([AState], AState))
    deriving (Show,Eq)

getNextId :: State Int StateId
getNextId = do
    num <- get
    put (num + 1)
    pure (StateId num)

getStatesMap :: [ECState] -> StateMap
getStatesMap basicStates =
    StateMap
        (Map.fromList
             (zip
                  (map ecStateName basicStates)
                  (evalState (mapM getLocationsFromState basicStates) 0)))

-- | Calculates the states from each input state
--
-- If there are no actions for the state, then the initState &
-- actionStates will both be empty.
getLocationsFromState :: ECState -> State Int ([AState],AState)
getLocationsFromState state = do
    let endCoord = ecStatePosition state
    let offset = 30
    let startCoord =
            endCoord -
            (offset * fromIntegral (length (ecStateActions state) + 1) :+ 0)
    initState <-
        if null (ecStateActions state)
            then pure mempty
            else fmap
                     (: mempty)
                     (createState locationStartPrefix startCoord state)
    actionStates <-
        zipWithM
            (createState (locationEventPrefix state))
            (tail (iterate (+ (offset :+ 0)) startCoord))
            (ecStateActions state)
    destState <- createState ecStateName endCoord state
    pure (initState <> actionStates, destState)

locationsToMap :: [Location] -> Map String StateId
locationsToMap lst = Map.fromList (map f lst)
  where
    f (UrgentLocation (AState s i _)) = (s, i)
    f (Location (AState s i _)) = (s, i)

advancedTransitions :: Map String StateId -> ECState -> [Transition]
advancedTransitions m s
  | null (ecStateActions s) = mempty
  | otherwise = map makeTransition trTriples
  where
    makeTransition (act,a,b) =
        Transition
            (m ! a)
            (m ! b)
            (makeSyncStatement act)
            -- Guard is always empty for advanced transitions.
            mzero
            (makeUpdateStatement act)
    emptyAction = ECAction mempty mempty
    acts = ecStateActions s <> repeat emptyAction
    trTriples = zip3 acts trSrcs (tail trSrcs)
    trSrcs =
        locationStartPrefix s :
        map (locationEventPrefix s) (ecStateActions s) <> [ecStateName s]

makeUpdateStatement :: MonadPlus m => ECAction -> m String
makeUpdateStatement action
  | null (ecActionAlgorithm action) = mzero
  | otherwise = pure (ecActionAlgorithm action <> "();")

makeSyncStatement :: MonadPlus m => ECAction -> m String
makeSyncStatement action
  | null (ecActionOutput action) = mzero
  | otherwise = pure (outputChannelPrefix <> ecActionOutput action <> "!")

createState :: (t -> String) -> Complex Float -> t -> State Int AState
createState f coord x = do
    nextId <- getNextId
    pure (AState (f x) nextId coord)

getBasicStates :: BasicFunctionBlock -> [ECState]
getBasicStates = bfbStates

transitions :: BasicFunctionBlock -> Either String [Transition]
transitions fb = mappend <$> basicTransitions <*> pure otherTransitions
  where
    states = getBasicStates fb
    statesMap = getStatesMap states
    -- Transitions which are defined in the input BasicFunctionBlock.
    basicTransitions :: Either String [Transition]
    basicTransitions = traverse createBasicTransition (bfbTransitions fb)
    -- Transitions which are required to handle the urgent
    -- locations.  The list of required transitions is one
    -- transition from urgent state to the next and then one final
    -- transition to the end state.
    otherTransitions =
        foldMap (advancedTransitions (locationsToMap (locations fb))) states
    events = Set.fromList (map eventName (eventInputs (bfbInterfaceList fb)))
    createBasicTransition :: ECTransition -> Either String Transition
    createBasicTransition (ECTransition src dest cond _) =
        Transition <$> pure (getSrcId src statesMap)
                   <*> pure (getDestId dest statesMap)
                   <*> fmap guardToSync eitherGuard
                   <*> (guardToGuard =<< eitherGuard)
                   <*> pure mzero -- No update/advancedTransitions on the basic transition.
      where
        eitherGuard =
            maybe
                (Left ("Couldn't parse guard: " <> show cond))
                Right
                (parseGuard events cond)

guardToSync :: MonadPlus m => Guard -> m String
guardToSync g =
    case g of
        Guard (Just s) _ -> pure (inputChannelPrefix <> s <> "?")
        _ -> mzero

guardToGuard :: MonadPlus m => Guard -> Either String (m String)
guardToGuard g =
    case g of
        Guard _ (Just (StBool True)) -> pure mzero
        Guard _ (Just (StInt 1)) -> pure mzero
        Guard _ (Just v) -> fmap pure (showValue v)
        _ -> pure mzero

getSrcId :: String -> StateMap -> StateId
getSrcId s (StateMap m) =
    let (AState _ i _) = snd (m ! s)
    in i

getDestId :: String -> StateMap -> StateId
getDestId s (StateMap m)
  | null (fst (m ! s)) = getSrcId s (StateMap m)
  | otherwise =
      let (AState _ i _) = head (fst (m ! s))
      in i

type AlgorithmWriter = ReaderT Int (WriterT (DList String) (Except String)) ()

anAlgorithm :: ECAlgorithm -> Either String String
anAlgorithm al = right fold (runExcept (execWriterT (runReaderT writeFunction 0)))
  where
    writeFunction = do
        writeLine ("void " <> ecAlgorithmName al <> "()")
        writeBlock (ecAlgorithmStText al)
        -- Append blank line for formatting.
        writeLine mempty

writeLine :: String -> AlgorithmWriter
writeLine l = do
    n <- ask
    tell (DList.singleton (replicate n '\t' <> l <> "\n"))

increaseIndent :: ReaderT Int m a -> ReaderT Int m a
increaseIndent = withReaderT (1 +)

writeBlock :: [Statement] -> AlgorithmWriter
writeBlock statements = do
    writeLine "{"
    increaseIndent (traverse_ writeStatement statements)
    writeLine "}"

tryWriteLine :: Either String t -> (t -> String) -> AlgorithmWriter
tryWriteLine v f = case v of
  Right s -> writeLine $ f s
  Left e -> throwError e

tryWriteLine2 :: Either String t1 -> Either String t2 -> (t1 -> t2 -> String) -> AlgorithmWriter
tryWriteLine2 v1 v2 f = case (v1, v2) of
  (Right l, Right r) -> writeLine $ f l r
  (Left e, _) -> throwError e
  (_, Left e) -> throwError e

writeStatement :: Statement -> AlgorithmWriter
writeStatement st =
    case st of
        Declaration name typeIn -> do
            tryWriteLine
                (showVarType typeIn)
                (\(typeOut,suffix) ->
                      typeOut <> " " <> name <> suffix <> ";")
        Assignment lvalue rvalue ->
            tryWriteLine2
                (showLocation lvalue)
                (showValue rvalue)
                (\l r ->
                      l <> " = " <> r <> ";")
        For name start end step body -> do
            writeLine
                ("for (int " <> name <> " = " <> show start <> "; " <> name <>
                 " != " <> show end <> "; " <> name <> " = " <>
                 name <> " + (" <> show (fromMaybe 1 step) <> "))")
            writeBlock body
        While cond body -> do
            tryWriteLine
                (showValue cond)
                (\s -> "while (" <> s <> ")")
            writeBlock body
        Repeat body cond -> do
            writeLine "do"
            writeBlock body
            tryWriteLine
                (showValue cond)
                (\x -> "while (" <> x <> ")")
        Case var branches defaultBranch -> do
            tryWriteLine
                (showValue var)
                (\x -> "case (" <> x <> ")")
            writeLine "{"
            traverse_ writeBranch branches
            writeDefaultBranch
            writeLine "}"
            where writeCase i = writeLine (i <> ":")
                  writeCases = traverse_ (writeCase . show)
                  writeCaseExp (CaseInt i) = writeCase (show i)
                  writeCaseExp (CaseRange from to)
                    | from <= to = writeCases (enumFromTo from to)
                    | otherwise =
                        writeCases (enumFromThenTo from (from - 1) to)
                  writeBranch (cases,body) = do
                      traverse_ writeCaseExp cases
                      increaseIndent $
                          do traverse_ writeStatement body
                             writeStatement Break
                  writeDefaultBranch = do
                      writeCase "default"
                      increaseIndent $
                          do traverse_ writeStatement defaultBranch
                             writeStatement Break
        If cond branch -> do
            tryWriteLine
                (showValue cond)
                (\x -> "if (" <> x <> ")")
            writeBlock branch
        IfElse cond branch1 branch2 -> do
            tryWriteLine
                (showValue cond)
                (\x -> "if (" <> x <> ")")
            writeBlock branch1
            writeLine "else"
            writeBlock branch2
        Break -> writeLine "break;"
        Return -> writeLine "return;"

showArgs :: [Value] -> Either String String
showArgs = right (fold . intersperse ", ") . traverse showValue

showValue :: Value -> Either String String
showValue value =
    case value of
        StSubValue values ->
            right
                (\x -> "(" <> x <> ")")
                (showValue values)
        StBool True -> Right "true"
        StBool False -> Right "false"
        StBinaryOp op a b -> showBinaryValue a op b
        StMonoOp op a -> showMonoValue op a
        StTime t -> Right $ show t
        StInt i -> Right $ show i
        StLValue v -> showLocation v
        -- This entry will not be required once the float -> int conversion
        -- is working correctly.
        StFloat i -> Right $ showFFloat Nothing i ""
        StFunc name args ->
            right
                (\x -> name <> "(" <> x <> ")")
                (showArgs args)
        StChar c -> Right $ show (ord c)

showBinaryValue :: Value -> StBinaryOp -> Value -> Either String String
showBinaryValue a op b = right fold (sequence (createString opStr))
  where
    createString x = [showValue a, pure " ", x, pure " ", showValue b]
    opStr =
        case op of
            StAddition -> Right "+"
            StSubtract -> Right "-"
            StExp -> Left "Uppaal doesn't support exponentiation!"
            StMultiply -> Right "*"
            StDivide -> Right "/"
            StEquals -> Right "="
            StNotEquals -> Right "!="
            StLessThanEquals -> Right "<="
            StLessThan -> Right "<"
            StGreaterThanEquals -> Right ">="
            StGreaterThan -> Right ">"
            StMod -> Right "%"
            StBitwiseAnd -> Right "&"
            StAnd -> Right "&&"
            StOr -> Right "||"
            StXor -> Right "^"

showMonoValue :: StMonoOp -> Value -> Either String String
showMonoValue op v =
    case op of
        StNegate -> mappend <$> pure "-" <*> showValue v
        StNot -> mappend <$> pure "!" <*> showValue v

showLocation :: LValue -> Either String String
showLocation value =
    case value of
        SimpleLValue name -> Right name
        ArrayLValue name idx -> mappend <$> pure name <*> showValue idx

createLibraryFunctions :: BasicFunctionBlock -> String
createLibraryFunctions = foldMap f . fbFunctions
  where
    f str = "void " <> str <> "()\n{\n}\n\n"

fbFunctions :: BasicFunctionBlock -> Set String
fbFunctions fb =
    foldMap
        (extractFunctionStatement . ecAlgorithmStText)
        (bfbAlgorithms fb) <>
    foldMap
        (extractFunctionValue . ecTransitionCondition)
        (bfbTransitions fb)

-- | Extracts the functions from the supplied statements.
extractFunctionStatement :: Foldable t => t Statement -> Set String
extractFunctionStatement = foldMap statement
  where
    statement (Assignment lv v) = extractFunctionLValue lv <> extractFunctionValue v
    statement (If v tb) = extractFunctionValue v <> extractFunctionStatement tb
    statement (IfElse v tb eb) =
        extractFunctionValue v <> extractFunctionStatement tb <>
        extractFunctionStatement eb
    statement (For _ _ _ _ sts) = extractFunctionStatement sts
    statement (While v sts) = extractFunctionValue v <> extractFunctionStatement sts
    statement (Repeat sts v) = extractFunctionValue v <> extractFunctionStatement sts
    statement (Case v sts eb) =
        extractFunctionValue v <> extractFunctionStatement (foldMap snd sts) <>
        extractFunctionStatement eb
    statement _ = mempty

extractFunctionLValue :: LValue -> Set String
extractFunctionLValue lv =
    case lv of
        SimpleLValue _ -> mempty
        ArrayLValue _ v -> extractFunctionValue v

extractFunctionValue :: Value -> Set String
extractFunctionValue val =
    case val of
        StMonoOp _ v -> extractFunctionValue v
        StBinaryOp _ v1 v2 -> extractFunctionValue v1 <> extractFunctionValue v2
        StLValue lv -> extractFunctionLValue lv
        StFunc s vs -> Set.singleton s <> foldMap extractFunctionValue vs
        StSubValue v -> extractFunctionValue v
        _ -> mempty
