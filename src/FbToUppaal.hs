module FbToUppaal (fbToUppaalModel) where

import           BasePrelude
import           Control.Monad.Trans.State.Lazy (State, evalState, get, put)
import           Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           OutputUppaal
       (UppaalModel(..), AState(..), UppaalChan(..), UppaalVar(..),
        Location(..), StateId(..), Transition(..))
import           ParseGuard (Guard(..), GuardCondition(..), parseGuard)
import           ParseIec61499
       (ECTransition(..), ECState(..), FunctionBlock(..), IECVariable(..),
        InterfaceList(..), Event(..), Variable(..), ECAction(..),
        BasicFunctionBlock(..), ECAlgorithm(..))
import           ParseSt (Statement(..), Symbol(..))

fbToUppaalModel :: FunctionBlock -> UppaalModel
fbToUppaalModel fb =
    UppaalModel
        (fbName fb)
        (inputChannels fb)
        (outputChannels fb)
        (inputParameters fb)
        (outputParameters fb)
        (locations fb)
        (transitions fb)
        (fmap anAlgorithm (bfbAlgorithms (basicFb fb)))

--------------------------------------------------------------------------------
-- Handle events

extractChannels :: String -> (InterfaceList -> [Event]) -> FunctionBlock -> [UppaalChan]
extractChannels prefix f =
    fmap (UppaalChan . (prefix <>) . eventName) . f . interfaceList

inputChannelPrefix :: String
inputChannelPrefix = "ic_"

outputChannelPrefix :: String
outputChannelPrefix = "oc_"

inputChannels :: FunctionBlock -> [UppaalChan]
inputChannels = extractChannels inputChannelPrefix eventInputs

outputChannels :: FunctionBlock -> [UppaalChan]
outputChannels = extractChannels outputChannelPrefix eventOutputs

--------------------------------------------------------------------------------
-- Handle variables

convertVariableType :: IECVariable -> String
convertVariableType IECBool = "bool"
convertVariableType IECInt = "int"
convertVariableType IECReal = error "Uppaal doesn't support Real types!"

inputParameters :: FunctionBlock -> [UppaalVar]
inputParameters = fmap createUppaalVar . inputVariables . interfaceList

outputParameters :: FunctionBlock -> [UppaalVar]
outputParameters = fmap createUppaalVar . outputVariables . interfaceList

createUppaalVar :: Variable -> UppaalVar
createUppaalVar var =
    UppaalVar (convertVariableType (variableType var)) (variableName var)

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
    "__action_" <> ecStateName state <> "__" <> ecActionOutput action <> "_" <>
    ecActionAlgorithm action <>
    "_"

locations :: FunctionBlock -> [Location]
locations fb = doFold states
  where
    states = getStatesMap (getBasicStates fb)
    doFold (StateMap m) = foldMap f m
    f (u,n) = fmap UrgentLocation u <> [Location n]

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
                  (fmap ecStateName basicStates)
                  (evalState (mapM getLocationsFromState basicStates) 0)))

getLocationsFromState :: ECState -> State Int ([AState],AState)
getLocationsFromState state = do
    initState <-
        if null (ecStateActions state)
            then pure mempty
            else fmap (: mempty) (createState locationStartPrefix state)
    actionStates <-
        mapM (createState (locationEventPrefix state)) (ecStateActions state)
    destState <- createState ecStateName state
    pure (initState <> actionStates, destState)

locationsToMap :: [Location] -> Map String StateId
locationsToMap lst = Map.fromList (fmap f lst)
  where
    f (UrgentLocation (AState s i)) = (s, i)
    f (Location (AState s i)) = (s, i)

advancedTransitions :: Map String StateId -> ECState -> [Transition]
advancedTransitions m s
  | null (ecStateActions s) = mempty
  | otherwise = fmap makeTransition trTriples
  where
    makeTransition (act,a,b) =
        Transition
            (m ! a)
            (m ! b)
            (makeSyncStatement act)
            mzero -- Guard is always empty for advanced transitions.
            (makeUpdateStatement act)
    emptyAction = ECAction mempty mempty
    acts = ecStateActions s <> repeat emptyAction
    trTriples = zip3 acts trSrcs (tail trSrcs)
    trSrcs =
        locationStartPrefix s :
        fmap (locationEventPrefix s) (ecStateActions s) <> [ecStateName s]

makeUpdateStatement :: MonadPlus m => ECAction -> m String
makeUpdateStatement action
  | null (ecActionAlgorithm action) = mzero
  | otherwise = pure (ecActionAlgorithm action <> "();")

makeSyncStatement :: MonadPlus m => ECAction -> m String
makeSyncStatement action
  | null (ecActionOutput action) = mzero
  | otherwise = pure (outputChannelPrefix <> ecActionOutput action <> "!")

createState :: (t -> String) -> t -> State Int AState
createState f x = do
    nextId <- getNextId
    pure (AState (f x) nextId)

getBasicStates :: FunctionBlock -> [ECState]
getBasicStates = bfbStates . basicFb

transitions :: FunctionBlock -> [Transition]
transitions fb = basicTransitions <> otherTransitions
  where
    states = getBasicStates fb
    statesMap = getStatesMap states
    -- Transitions which are defined in the input FunctionBlock.
    basicTransitions = fmap createBasicTransition (bfbTransitions (basicFb fb))
    -- Transitions which are required to handle the urgent
    -- locations.  The list of required transitions is one
    -- transition from urgent state to the next and then one final
    -- transition to the end state.
    otherTransitions =
        foldMap (advancedTransitions (locationsToMap (locations fb))) states
    events = Set.fromList (fmap eventName (eventInputs (interfaceList fb)))
    createBasicTransition (ECTransition src dest cond) =
        Transition
            (getSrcId src statesMap)
            (getDestId dest statesMap)
            (guardToSync gd)
            (guardToGuard gd)
            mzero -- No update/advancedTransitions on the basic transition.
      where
        gd =
            either
                (const (error ("Couldn't parse guard: " <> cond)))
                id
                (parseGuard events cond)

guardToSync :: Guard -> Maybe String
guardToSync (Guard (Just s) _) = pure (inputChannelPrefix <> s <> "?")
guardToSync _ = mzero

guardToGuard :: Guard -> Maybe String
guardToGuard (Guard _ (Just (GuardSubCondition [GuardTrue]))) = mzero
guardToGuard (Guard _ (Just (GuardSubCondition e))) = pure (foldMap f e)
  where
    f (GuardAnd) = " & "
    f (GuardOr) = " | "
    f (GuardNot) = "!"
    f (GuardTrue) = "true"
    f (GuardFalse) = "false"
    f (GuardVariable var) = var
    f (GuardSubCondition child) = "(" <> foldMap f child <> ")"
guardToGuard _ = mzero

getSrcId :: String -> StateMap -> StateId
getSrcId s (StateMap m) =
    let (AState _ i) = snd (m ! s)
    in i

getDestId :: String -> StateMap -> StateId
getDestId s (StateMap m)
  | null (fst (m ! s)) = getSrcId s (StateMap m)
  | otherwise =
      let (AState _ i) = head (fst (m ! s))
      in i

anAlgorithm :: ECAlgorithm -> String
anAlgorithm al =
    "void " <> ecAlgorithmName al <> "()\n{\n" <>
    foldMap st (ecAlgorithmStText al) <>
    "}\n"
  where
    st (Assignment lvalue rvalue) =
        "\t" <> lvalue <> " = " <>
        mconcat (intersperse " " (fmap showSymbol rvalue)) <>
        ";\n"
    showSymbol (StBool True) = "true"
    showSymbol (StBool False) = "false"
    showSymbol (StVar str) = str
    showSymbol (StInt i) = show i
