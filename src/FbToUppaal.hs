module FbToUppaal (fbToUppaalModel, anAlgorithm) where

import           BasePrelude
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ask, runReaderT, withReaderT)
import           Control.Monad.Trans.State.Lazy (State, evalState, get, put)
import           Control.Monad.Trans.Writer.Lazy (execWriter, tell)
import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           OutputUppaal
       (UppaalModel(..), AState(..), UppaalChan(..), UppaalVar(..),
        Location(..), StateId(..), Transition(..))
import           ParseGuard (Guard(..), GuardCondition(..), parseGuard)
import           ParseIec61499
       (ECTransition(..), ECState(..), FunctionBlock(..),
        InterfaceList(..), Event(..), Variable(..), ECAction(..),
        BasicFunctionBlock(..), ECAlgorithm(..))
import           ParseSt
       (LValue(..), Statement(..), Value(..), IECVariable(..), Width(..),
        CaseSubExpression(..))

-- | Converts IEC61499 FunctionBlock to an UppaalModel
-- If something goes wrong then an exception is thrown.
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

showVarType :: IECVariable -> (String,String)
showVarType IECBool = ("bool",mempty)
showVarType (IECUInt Eight) = (intWithRange 0 ((2::Integer)^(8::Integer)),mempty)
showVarType (IECUInt Sixteen) = (intWithRange 0 ((2::Integer)^(16::Integer)),mempty)
showVarType (IECUInt ThirtyTwo) = (intWithRange 0 ((2::Integer)^(32::Integer)),mempty)
showVarType (IECUInt SixtyFour) = (intWithRange 0 ((2::Integer)^(64::Integer)),mempty)
showVarType (IECInt Eight) = (intWithRange ((-2::Integer)^(7::Integer)) ((2::Integer)^(7::Integer)-1),mempty)
showVarType (IECInt Sixteen) = (intWithRange ((-2::Integer)^(15::Integer)) ((2::Integer)^(15::Integer)-1),mempty)
showVarType (IECInt ThirtyTwo) = (intWithRange ((-2::Integer)^(31::Integer)) ((2::Integer)^(31::Integer)-1),mempty)
showVarType (IECInt SixtyFour) = (intWithRange ((-2::Integer)^(63::Integer)) ((2::Integer)^(63::Integer)-1),mempty)
showVarType (IECArray idxs var) = (fst (showVarType var), "[" <> foldMap id (NE.intersperse "," (fmap show idxs)) <> "]")
showVarType t = error ("Uppaal doesn't support " <> show t <> " type!")

intWithRange :: Integer -> Integer -> String
intWithRange from to = "int[" <> show from <> "," <> show to <> "]"

inputParameters :: FunctionBlock -> [UppaalVar]
inputParameters = fmap createUppaalVar . inputVariables . interfaceList

outputParameters :: FunctionBlock -> [UppaalVar]
outputParameters = fmap createUppaalVar . outputVariables . interfaceList

createUppaalVar :: Variable -> UppaalVar
createUppaalVar var = UppaalVar varType (variableName var <> suffix)
    where (varType,suffix) = showVarType (variableType var)

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
locationEventPrefix state action = fold ["__action_"
                                        ,(ecStateName state)
                                        ,"__"
                                        ,(ecActionOutput action)
                                        ,"_"
                                        ,(ecActionAlgorithm action)
                                        ,"_"
                                        ]

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

guardToSync :: MonadPlus m => Guard -> m String
guardToSync (Guard (Just s) _) = pure (inputChannelPrefix <> s <> "?")
guardToSync _ = mzero

guardToGuard :: MonadPlus m => Guard -> m String
guardToGuard (Guard _ (Just (GuardSubCondition [GuardTrue]))) = mzero
guardToGuard (Guard _ (Just (GuardSubCondition e))) = pure (foldMap f e)
  where
    f (GuardEquals) = " = "
    f (GuardApprox) = " <> " --TODO This is wrong!
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
anAlgorithm al = fold (execWriter (runReaderT writeFunction 0))
  where
    -- The writer monad uses a DList so writeLine runs in constant
    -- time; the 'foldMap (<>"\n)' simultaneously adds the newlines
    -- and flattens the lists into one string.
    writeLine l = do
      n <- ask
      lift (tell (DList.singleton (replicate n '\t' <> l <> "\n")))
    writeFunction = do
      writeLine ("void " <> ecAlgorithmName al <> "()")
      writeBlock (ecAlgorithmStText al)
    increaseIndent =
      withReaderT (1 +)
    writeBlock statements = do
      writeLine "{"
      increaseIndent (traverse_ writeStatement statements)
      writeLine "}"
    writeStatement (Declaration name typeIn) =
      writeLine (typeOut <> " " <> name <> suffix <> ";")
      where (typeOut, suffix) = showVarType typeIn
    writeStatement (Assignment lvalue rvalue) =
      writeLine (showLocation lvalue <> " = " <> showValue rvalue <> ";")
    writeStatement (For name start end step body) = do
      writeLine
        ("for (int " <> name <> " = " <> show start <> "; " <>
         name <> " != " <> show end <> "; " <>
         name <> " = " <> name <> " + (" <>
         show (fromMaybe 1 step) <> "))")
      writeBlock body
    writeStatement (While cond body) = do
      writeLine ("while (" <> showValue cond <> ")")
      writeBlock body
    writeStatement (Repeat body cond) = do
      writeLine "do"
      writeBlock body
      writeLine ("while (" <> showValue cond <> ")")
    writeStatement (Case var branches defaultBranch) = do
      writeLine ("case (" <> showValue var <> ")")
      writeLine "{"
      traverse_ writeBranch branches
      writeDefaultBranch
      writeLine "}"
      where
        writeCase i = writeLine (i <> ":")
        writeCases = traverse_ (writeCase . show)
        writeCaseExp (CaseInt i) = writeCase (show i)
        writeCaseExp (CaseRange from to)
          | from <= to = writeCases (enumFromTo from to)
          | otherwise = writeCases (enumFromThenTo from (from-1) to)
        writeBranch (cases, body) = do
          traverse_ writeCaseExp cases
          increaseIndent $ do traverse_ writeStatement body
                              writeStatement Break
        writeDefaultBranch = do
          writeCase "default"
          increaseIndent $ do traverse_ writeStatement defaultBranch
                              writeStatement Break
    writeStatement (If cond branch) = do
      writeLine ("if (" <> showValue cond <> ")")
      writeBlock branch
    writeStatement (IfElse cond branch1 branch2) = do
      writeLine ("if (" <> showValue cond <> ")")
      writeBlock branch1
      writeLine "else"
      writeBlock branch2
    writeStatement Break = writeLine "break;"
    writeStatement Return = writeLine "return;"
    showArgs = fold . intersperse ", " . fmap showValue
    showValue (StSubValue values) = "(" <> showValue values <> ")"
    showValue (StBool True) = "true"
    showValue (StBool False) = "false"
    showValue (StAddition a b) = showBinaryValue a " + " b
    showValue (StSubtract a b) = showBinaryValue a " - " b
    showValue (StMultiply a b) = showBinaryValue a " * " b
    showValue (StDivide a b) = showBinaryValue a " / " b
    showValue (StTime t) = show t
    showValue (StInt i) = show i
    showValue (StLValue v) = showLocation v
    showValue (StFloat i) = show i -- TODO Uppaal can’t handle floats!
    showValue (StFunc name args) = name <> "(" <> showArgs args <> ")"
    showBinaryValue a op b = showValue a <> op <> showValue b
    showLocation (SimpleLValue name) = name
    showLocation (ArrayLValue name idx) = name <> showValue idx
