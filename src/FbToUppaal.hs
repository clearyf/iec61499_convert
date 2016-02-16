{-# LANGUAGE FlexibleContexts #-}

module FbToUppaal (fbToUppaalModel) where

import           BasePrelude
import           Control.Monad.Except (MonadError, throwError, runExcept)
import           Control.Monad.Reader (MonadReader, asks, runReaderT)
import           Control.Monad.State (State, evalState, get, put)
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
import           ParseSt (LValue(..), Statement(..), Value(..))
import           StToUppaal (stToUppaal, showValue, createUppaalVar)

-- | Converts IEC61499 BasicFunctionBlock to an UppaalModel
fbToUppaalModel :: BasicFunctionBlock -> Either String UppaalModel
fbToUppaalModel fb = runExcept (runReaderT createModel fb)
  where
    createModel =
        UppaalModel <$> asks (fbName . bfbDescription)
                    <*> extractChannels inputChannelPrefix eventInputs
                    <*> extractChannels outputChannelPrefix eventOutputs
                    <*> extractParameters (inputVariables . bfbInterfaceList)
                    <*> extractParameters (outputVariables . bfbInterfaceList)
                    <*> extractParameters bfbVariables
                    <*> extractLocations
                    <*> extractTransitions
                    <*> extractDefinitions

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
    f (States actions end) =
        map UrgentLocation actions <> [Location end]

-- | Calculates the states from each input state
--
-- If there are no actions for the state, then the initState &
-- actionStates will both be empty.
getLocationsFromState :: ECState -> LocationIdState States
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
    actions <-
        zipWithM
            (createState (locationEventPrefix state))
            (tail (iterate (+ (offset :+ 0)) startCoord))
            (ecStateActions state)
    dest <- createState ecStateName endCoord state
    pure $! States (initState <> actions) dest

-- | Simple typedef to make in clearer what the state is.  Only
-- getNextId should be used to peek at the state.
type LocationIdState = State Int

getNextId :: LocationIdState StateId
getNextId = do
    num <- get
    put (num + 1)
    pure (StateId num)

data States = States
    { actionStates :: [AState]
    , destState :: AState
    } deriving (Show,Eq)

newtype StateMap =
    StateMap (Map String States)
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
    makeTransition action src dest =
        Transition
            (stateMap ! src)
            (stateMap ! dest)
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
    case destState (m ! s) of
        (AState _ i _) -> i

getDestId :: String -> StateMap -> StateId
getDestId s (StateMap m) =
    case actionStates (m ! s) of
        [] -> getSrcId s (StateMap m)
        (AState _ i _):_ -> i

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
    showedAlgorithms <- traverse stToUppaal algorithms
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
