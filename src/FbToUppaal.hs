{-# LANGUAGE FlexibleContexts #-}

module FbToUppaal (fbToUppaalModel, anAlgorithm) where

import           BasePrelude
import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad.Reader.Class (MonadReader, ask, asks, local)
import           Control.Monad.State.Class (get, put)
import           Control.Monad.Trans.Except (runExcept)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Trans.State.Strict (State, evalState)
import           Control.Monad.Trans.Writer.Lazy (execWriterT)
import           Control.Monad.Writer.Class (MonadWriter, tell)
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
fbToUppaalModel fb = runExcept (runReaderT createModel fb)
  where
    createModel = do
        name <- asks (fbName . bfbDescription)
        inputChannels <- extractChannels inputChannelPrefix eventInputs
        outputChannels <- extractChannels outputChannelPrefix eventOutputs
        inputParameters <-
            extractParameters (inputVariables . bfbInterfaceList)
        outputParameters <-
           extractParameters (outputVariables . bfbInterfaceList)
        localParameters <- extractParameters bfbVariables
        locations <- extractLocations
        transitions <- extractTransitions
        definitions <- extractDefinitions
        pure $!
            UppaalModel
                name
                inputChannels
                outputChannels
                inputParameters
                outputParameters
                localParameters
                locations
                transitions
                definitions

--------------------------------------------------------------------------------
-- Handle events

extractChannels :: MonadReader BasicFunctionBlock m => String -> (InterfaceList -> [Event]) -> m [UppaalChan]
extractChannels prefix f = do
    channels <- asks (f . bfbInterfaceList)
    pure $! map (UppaalChan . (prefix <>) . eventName) channels

inputChannelPrefix :: String
inputChannelPrefix = "ic_"

outputChannelPrefix :: String
outputChannelPrefix = "oc_"

--------------------------------------------------------------------------------
-- Handle variables

extractParameters :: (Traversable t, MonadReader r m, MonadError String m) => (r -> t Variable) -> m (t UppaalVar)
extractParameters f = do
    parameters <- asks f
    traverse createUppaalVar parameters

createUppaalVar :: MonadError String m => Variable -> m UppaalVar
createUppaalVar var = do
    (typeName,arrayDimensions) <- showVarType (variableType var)
    pure $! UppaalVar typeName (variableName var <> arrayDimensions)

-- | Used by createUppaalVar to create the correct variable
-- declarations.
showVarType :: MonadError String m => IECVariable -> m (String,String)
showVarType v =
    case v of
        IECBool -> pure ("bool", mempty)
        IECUInt Eight -> pure $ uintWithRange 8
        IECUInt Sixteen -> pure $ uintWithRange 16
        IECUInt ThirtyTwo -> pure $ uintWithRange 32
        IECUInt SixtyFour -> pure $ uintWithRange 64
        IECInt Eight -> pure $ intWithRange 7
        IECInt Sixteen -> pure $ intWithRange 15
        IECInt ThirtyTwo -> pure $ intWithRange 31
        IECInt SixtyFour -> pure $ intWithRange 63
        IECArray idxs var -> do
            (x,_) <- showVarType var
            let y = "[" <> fold (NE.intersperse "," (fmap show idxs)) <> "]"
            pure (x, y)
        IECString size ->
            pure (uppaalIntWithRange 0 127, "[" <> show size <> "]")
        t -> throwError ("Uppaal doesn't support " <> show t <> " type!")

uppaalIntWithRange :: Integer -> Integer -> String
uppaalIntWithRange from to = "int[" <> show from <> "," <> show to <> "]"

intWithRange :: Integer -> (String, String)
intWithRange i = (uppaalIntWithRange ((-2) ^ i) (2 ^ i - 1), mempty)

uintWithRange :: Integer -> (String, String)
uintWithRange i = (uppaalIntWithRange 0 (2 ^ i), mempty)

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

-- | Extracts the list of locations from the BasicFunctionBlock.
extractLocations :: MonadReader BasicFunctionBlock m => m [Location]
extractLocations = do
    (StateMap m) <- getStatesMap
    pure $! foldMap f m
  where
    f (actionStates,endState) =
        map UrgentLocation actionStates <> [Location endState]

-- | Calculates the states from each input state
--
-- If there are no actions for the state, then the initState &
-- actionStates will both be empty.
getLocationsFromState :: ECState -> LocationIdState ([AState],AState)
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

-- | Simple typedef to make in clearer what the state is.  Only
-- getNextId should be used to peek at the state.
type LocationIdState = State Int

getNextId :: LocationIdState StateId
getNextId = do
    num <- get
    put (num + 1)
    pure (StateId num)

newtype StateMap =
    StateMap (Map String ([AState], AState))
    deriving (Show,Eq)

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

getStatesMap :: MonadReader BasicFunctionBlock m => m StateMap
getStatesMap = do
    basicStates <- asks bfbStates
    pure $!
        StateMap
            (Map.fromList
                 (zip
                      (map ecStateName basicStates)
                      (evalState (mapM getLocationsFromState basicStates) 0)))

createState :: (t -> String) -> Complex Float -> t -> LocationIdState AState
createState f coord x = do
    nextId <- getNextId
    pure (AState (f x) nextId coord)

--------------------------------------------------------------------------------
-- Transitions

extractTransitions :: (MonadReader BasicFunctionBlock m, MonadError String m) => m [Transition]
extractTransitions = do
    -- Transitions which are defined in the input BasicFunctionBlock.
    basicTransitions <- getBasicTransitions
    -- Transitions which are required to handle the urgent
    -- locations.  The list of required transitions is one
    -- transition from urgent state to the next and then one final
    -- transition to the end state.
    advancedTransitions <- getAdvancedTransitions
    pure $! basicTransitions <> advancedTransitions

-- | Create a Map of state names to stateIds.  This includes _all_
-- states, both basic and advanced.
locationsToMap :: [Location] -> Map String StateId
locationsToMap lst = Map.fromList (map f lst)
  where
    f (UrgentLocation (AState s i _)) = (s, i)
    f (Location (AState s i _)) = (s, i)

getAdvancedTransitions :: (MonadReader BasicFunctionBlock m) => m [Transition]
getAdvancedTransitions = do
    locations <- extractLocations
    basicStates <- asks bfbStates
    pure $! foldMap (createAdvancedTransition (locationsToMap locations)) basicStates

createAdvancedTransition :: Map String StateId -> ECState -> [Transition]
createAdvancedTransition stateMap state =
    case ecStateActions state of
        [] -> mempty
        _ ->
            zipWith3
                makeTransition
                actions
                transitionSources
                (tail transitionSources)
  where
    makeTransition action srcState destState =
        Transition
            (stateMap ! srcState)
            (stateMap ! destState)
            (makeSyncStatement action)
            -- Guard is always empty for advanced transitions.
            mzero
            (makeUpdateStatement action)
    -- Add empty actions to the list of actions at least one extra
    -- transition will be added as the final destination state.
    emptyAction = ECAction mempty mempty
    actions = ecStateActions state <> repeat emptyAction
    transitionSources =
        locationStartPrefix state :
        map (locationEventPrefix state) (ecStateActions state) <>
        [ecStateName state]

makeUpdateStatement :: MonadPlus m => ECAction -> m String
makeUpdateStatement action =
    case ecActionAlgorithm action of
        [] -> mzero
        algorithm -> pure $! algorithm <> "();"

makeSyncStatement :: MonadPlus m => ECAction -> m String
makeSyncStatement action =
    case ecActionOutput action of
        [] -> mzero
        output -> pure $! outputChannelPrefix <> output <> "!"

getBasicTransitions
    :: (MonadReader BasicFunctionBlock m, MonadError String m)
    => m [Transition]
getBasicTransitions = do
    transitions <- asks bfbTransitions
    traverse createBasicTransition transitions
  where
    createBasicTransition (ECTransition src dest cond _) = do
        statesMap <- getStatesMap
        let srcId = getSrcId src statesMap
        let destId = getDestId dest statesMap
        events <- getEvents
        errGuard <-
            maybe
                (throwError ("Couldn't parse guard: " <> show cond))
                pure
                (parseGuard events cond)
        let syncGuard = guardToSync errGuard
        guardGuard <- guardToGuard errGuard
        pure $! Transition srcId destId syncGuard guardGuard mzero

getEvents :: MonadReader BasicFunctionBlock m => m (Set String)
getEvents = do
    events <- asks (eventInputs . bfbInterfaceList)
    pure $! Set.fromList (map eventName events)

guardToSync :: Guard -> Maybe String
guardToSync g =
    case g of
        Guard (Just s) _ -> Just (inputChannelPrefix <> s <> "?")
        _ -> Nothing

guardToGuard :: MonadError String m => Guard -> m (Maybe String)
guardToGuard g =
    case g of
        Guard _ (Just (StBool True)) -> pure Nothing
        Guard _ (Just (StInt 1)) -> pure Nothing
        Guard _ (Just v) -> fmap pure (showValue v)
        _ -> pure Nothing

getSrcId :: String -> StateMap -> StateId
getSrcId s (StateMap m) =
    let (AState _ i _) = snd (m ! s)
    in i

getDestId :: String -> StateMap -> StateId
getDestId s (StateMap m) =
    case fst (m ! s) of
        [] -> getSrcId s (StateMap m)
        actionStates ->
            let (AState _ i _) = head actionStates
            in i

anAlgorithm :: MonadError String m => ECAlgorithm -> m String
anAlgorithm al =
    fmap fold (execWriterT (runReaderT writeFunction 0))
  where
    writeFunction = do
        writeLine ("void " <> ecAlgorithmName al <> "()")
        writeBlock (ecAlgorithmStText al)
        -- Append blank line for formatting.
        writeLine
            mempty

writeLine
    :: (MonadReader Int m, MonadWriter (DList String) m)
    => String -> m ()
writeLine l = do
    n <- ask
    tell (DList.singleton (replicate n '\t' <> l <> "\n"))

increaseIndent :: MonadReader Int m => m a -> m a
increaseIndent = local (1 +)

writeBlock
    :: (MonadError String m, MonadWriter (DList String) m, MonadReader Int m)
    => [Statement] -> m ()
writeBlock statements = do
    writeLine "{"
    increaseIndent (traverse_ writeStatement statements)
    writeLine "}"

tryWriteLine
    :: (MonadReader Int m, MonadError e m, MonadWriter (DList String) m)
    => Either e t -> (t -> String) -> m ()
tryWriteLine v f = case v of
  Right s -> writeLine $ f s
  Left e -> throwError e

tryWriteLine2
    :: (MonadReader Int m, MonadError e m, MonadWriter (DList String) m)
    => Either e t1 -> Either e t2 -> (t1 -> t2 -> String) -> m ()
tryWriteLine2 v1 v2 f = case (v1, v2) of
  (Right l, Right r) -> writeLine $ f l r
  (Left e, _) -> throwError e
  (_, Left e) -> throwError e

writeStatement
    :: (MonadError String m, MonadWriter (DList String) m, MonadReader Int m)
    => Statement -> m ()
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

showArgs :: MonadError String m => [Value] -> m String
showArgs = fmap (fold . intersperse ", ") . traverse showValue

showValue :: MonadError String m => Value -> m String
showValue value =
    case value of
        StSubValue values ->
            fmap
                (\x -> "(" <> x <> ")")
                (showValue values)
        StBool True -> pure "true"
        StBool False -> pure "false"
        StBinaryOp op a b -> showBinaryValue a op b
        StMonoOp op a -> showMonoValue op a
        StTime t -> pure $ show t
        StInt i -> pure $ show i
        StLValue v -> showLocation v
        -- This entry will not be required once the float -> int conversion
        -- is working correctly.
        StFloat i -> pure $ showFFloat Nothing i ""
        StFunc name args ->
            fmap
                (\x -> name <> "(" <> x <> ")")
                (showArgs args)
        StChar c -> pure $ show (ord c)

showBinaryValue
    :: MonadError String m
    => Value -> StBinaryOp -> Value -> m String
showBinaryValue a op b = fmap fold (sequence (createString opStr))
  where
    createString x = [showValue a, pure " ", x, pure " ", showValue b]
    opStr =
        case op of
            StAddition -> pure "+"
            StSubtract -> pure "-"
            StExp -> throwError "Uppaal doesn't support exponentiation!"
            StMultiply -> pure "*"
            StDivide -> pure "/"
            StEquals -> pure "="
            StNotEquals -> pure "!="
            StLessThanEquals -> pure "<="
            StLessThan -> pure "<"
            StGreaterThanEquals -> pure ">="
            StGreaterThan -> pure ">"
            StMod -> pure "%"
            StBitwiseAnd -> pure "&"
            StAnd -> pure "&&"
            StOr -> pure "||"
            StXor -> pure "^"

showMonoValue :: MonadError String m => StMonoOp -> Value -> m String
showMonoValue op v =
    case op of
        StNegate -> mappend <$> pure "-" <*> showValue v
        StNot -> mappend <$> pure "!" <*> showValue v

showLocation :: MonadError String m => LValue -> m String
showLocation value =
    case value of
        SimpleLValue name -> pure name
        ArrayLValue name idx -> mappend <$> pure name <*> showValue idx

extractDefinitions
    :: (MonadError String m, MonadReader BasicFunctionBlock m)
    => m String
extractDefinitions = do
    algorithms <- extractAlgorithms
    libraryFunctions <- createLibraryFunctions
    pure (algorithms <> libraryFunctions)

extractAlgorithms
    :: (MonadError String m, MonadReader BasicFunctionBlock m)
    => m String
extractAlgorithms = do
    algorithms <- asks bfbAlgorithms
    showedAlgorithms <- traverse anAlgorithm algorithms
    pure $! fold showedAlgorithms

createLibraryFunctions :: MonadReader BasicFunctionBlock m => m String
createLibraryFunctions = do
    algorithmFunctions <-
        fmap (foldMap (extractFunctionStatement . ecAlgorithmStText)) $
        asks bfbAlgorithms
    guardFunctions <-
        fmap (foldMap (extractFunctionValue . ecTransitionCondition)) $
        asks bfbTransitions
    pure $! foldMap printFunction (algorithmFunctions <> guardFunctions)
  where
    printFunction str = "void " <> str <> "()\n{\n}\n\n"

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
