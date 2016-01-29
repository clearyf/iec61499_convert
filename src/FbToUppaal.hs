module FbToUppaal (fbToUppaalModel, anAlgorithm) where

import           BasePrelude
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT, withReaderT)
import           Control.Monad.Trans.State.Lazy (State, evalState, get, put)
import           Control.Monad.Trans.Writer.Lazy (WriterT, execWriter, tell)
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
        InterfaceList(..), Event(..), Variable(..), ECAction(..),
        BasicFunctionBlock(..), ECAlgorithm(..))
import           ParseSt
       (LValue(..), Statement(..), Value(..), IECVariable(..), Width(..),
        StMonoOp(..), StBinaryOp(..), CaseSubExpression(..))

-- | Converts IEC61499 BasicFunctionBlock to an UppaalModel
-- If something goes wrong then an exception is thrown.
fbToUppaalModel :: BasicFunctionBlock -> UppaalModel
fbToUppaalModel fb =
    UppaalModel
        (bfbName fb)
        (inputChannels fb)
        (outputChannels fb)
        (inputParameters fb)
        (outputParameters fb)
        (localParameters fb)
        (locations fb)
        (transitions fb)
        (foldMap anAlgorithm (bfbAlgorithms fb) <>
         createLibraryFunctions fb)

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

showVarType :: IECVariable -> (String,String)
showVarType IECBool = ("bool", mempty)
showVarType (IECUInt Eight) =
    (intWithRange 0 ((2 :: Integer) ^ (8 :: Integer)), mempty)
showVarType (IECUInt Sixteen) =
    (intWithRange 0 ((2 :: Integer) ^ (16 :: Integer)), mempty)
showVarType (IECUInt ThirtyTwo) =
    (intWithRange 0 ((2 :: Integer) ^ (32 :: Integer)), mempty)
showVarType (IECUInt SixtyFour) =
    (intWithRange 0 ((2 :: Integer) ^ (64 :: Integer)), mempty)
showVarType (IECInt Eight) =
    ( intWithRange
          ((-2 :: Integer) ^ (7 :: Integer))
          ((2 :: Integer) ^ (7 :: Integer) - 1)
    , mempty)
showVarType (IECInt Sixteen) =
    ( intWithRange
          ((-2 :: Integer) ^ (15 :: Integer))
          ((2 :: Integer) ^ (15 :: Integer) - 1)
    , mempty)
showVarType (IECInt ThirtyTwo) =
    ( intWithRange
          ((-2 :: Integer) ^ (31 :: Integer))
          ((2 :: Integer) ^ (31 :: Integer) - 1)
    , mempty)
showVarType (IECInt SixtyFour) =
    ( intWithRange
          ((-2 :: Integer) ^ (63 :: Integer))
          ((2 :: Integer) ^ (63 :: Integer) - 1)
    , mempty)
showVarType (IECArray idxs var) =
    ( fst (showVarType var)
    , "[" <> foldMap id (NE.intersperse "," (fmap show idxs)) <> "]")
showVarType (IECString size) = (intWithRange 0 127, "[" <> show size <> "]")
showVarType t = error ("Uppaal doesn't support " <> show t <> " type!")

intWithRange :: Integer -> Integer -> String
intWithRange from to = "int[" <> show from <> "," <> show to <> "]"

inputParameters :: BasicFunctionBlock -> [UppaalVar]
inputParameters = map createUppaalVar . inputVariables . bfbInterfaceList

outputParameters :: BasicFunctionBlock -> [UppaalVar]
outputParameters = map createUppaalVar . outputVariables . bfbInterfaceList

localParameters :: BasicFunctionBlock -> [UppaalVar]
localParameters = map createUppaalVar . bfbVariables

createUppaalVar :: Variable -> UppaalVar
createUppaalVar var = UppaalVar varType (variableName var <> suffix)
  where
    (varType,suffix) = showVarType (variableType var)

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
        , (ecStateName state)
        , "__"
        , (ecActionOutput action)
        , "_"
        , (ecActionAlgorithm action)
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
            (offset * (fromIntegral (length (ecStateActions state) + 1)) :+ 0)
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

transitions :: BasicFunctionBlock -> [Transition]
transitions fb = basicTransitions <> otherTransitions
  where
    states = getBasicStates fb
    statesMap = getStatesMap states
    -- Transitions which are defined in the input BasicFunctionBlock.
    basicTransitions = map createBasicTransition (bfbTransitions fb)
    -- Transitions which are required to handle the urgent
    -- locations.  The list of required transitions is one
    -- transition from urgent state to the next and then one final
    -- transition to the end state.
    otherTransitions =
        foldMap (advancedTransitions (locationsToMap (locations fb))) states
    events = Set.fromList (map eventName (eventInputs (bfbInterfaceList fb)))
    createBasicTransition (ECTransition src dest cond _) =
        Transition
            (getSrcId src statesMap)
            (getDestId dest statesMap)
            (guardToSync gd)
            (guardToGuard gd)
            mzero -- No update/advancedTransitions on the basic transition.
      where
        gd =
            fromMaybe
                (error ("Couldn't parse guard: " <> (show cond)))
                (parseGuard events cond)

guardToSync :: MonadPlus m => Guard -> m String
guardToSync (Guard (Just s) _) = pure (inputChannelPrefix <> s <> "?")
guardToSync _ = mzero

guardToGuard :: MonadPlus m => Guard -> m String
guardToGuard (Guard _ (Just (StBool True))) = mzero
guardToGuard (Guard _ (Just (StInt 1))) = mzero
guardToGuard (Guard _ (Just v)) = pure (showValue v)
guardToGuard _ = mzero

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

anAlgorithm :: ECAlgorithm -> String
anAlgorithm al = fold (execWriter (runReaderT writeFunction 0))
  where
    writeFunction = do
        writeLine ("void " <> ecAlgorithmName al <> "()")
        writeBlock (ecAlgorithmStText al)
        -- Append blank line for formatting.
        writeLine
            mempty

writeLine :: Monad m => String -> ReaderT Int (WriterT (DList String) m) ()
writeLine l = do
    n <- ask
    lift (tell (DList.singleton (replicate n '\t' <> l <> "\n")))

increaseIndent :: ReaderT Int m a -> ReaderT Int m a
increaseIndent = withReaderT (1 +)

writeBlock :: Monad m => [Statement] -> ReaderT Int (WriterT (DList String) m) ()
writeBlock statements = do
    writeLine "{"
    increaseIndent (traverse_ writeStatement statements)
    writeLine "}"

writeStatement :: Monad m => Statement -> ReaderT Int (WriterT (DList String) m) ()
writeStatement (Declaration name typeIn) =
    writeLine (typeOut <> " " <> name <> suffix <> ";")
  where
    (typeOut,suffix) = showVarType typeIn
writeStatement (Assignment lvalue rvalue) =
    writeLine (showLocation lvalue <> " = " <> showValue rvalue <> ";")
writeStatement (For name start end step body) = do
    writeLine
        ("for (int " <> name <> " = " <> show start <> "; " <> name <> " != " <>
         show end <> "; " <> name <> " = " <> name <> " + (" <>
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
      | otherwise = writeCases (enumFromThenTo from (from - 1) to)
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

showArgs :: [Value] -> String
showArgs = fold . intersperse ", " . map showValue

showValue :: Value -> String
showValue (StSubValue values) = "(" <> showValue values <> ")"
showValue (StBool True) = "true"
showValue (StBool False) = "false"
showValue (StBinaryOp op a b) = showBinaryValue a op b
showValue (StMonoOp op a) = showMonoValue op a
showValue (StTime t) = show t
showValue (StInt i) = show i
showValue (StLValue v) = showLocation v
-- This entry will not be required once the float -> int conversion is
-- working correctly.
showValue (StFloat i) = showFFloat Nothing i ""
showValue (StFunc name args) = name <> "(" <> showArgs args <> ")"
showValue (StChar c) = show (ord c)

showBinaryValue :: Value -> StBinaryOp -> Value -> String
showBinaryValue a op b = showValue a <> " " <> opStr <> " " <> showValue b
  where
    opStr =
        case op of
            StAddition -> "+"
            StSubtract -> "-"
            StExp -> error "Uppaal doesn't support exponentiation!"
            StMultiply -> "*"
            StDivide -> "/"
            StEquals -> "="
            StNotEquals -> "!="
            StLessThanEquals -> "<="
            StLessThan -> "<"
            StGreaterThanEquals -> ">="
            StGreaterThan -> ">"
            StMod -> "%"
            StBitwiseAnd -> "&"
            StAnd -> "&&"
            StOr -> "||"
            StXor -> "^"

showMonoValue :: StMonoOp -> Value -> String
showMonoValue StNegate v = "-" <> showValue v
showMonoValue StNot v = "!" <> showValue v

showLocation :: LValue -> String
showLocation (SimpleLValue name) = name
showLocation (ArrayLValue name idx) = name <> showValue idx

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
extractFunctionStatement lst = foldMap statement lst
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
extractFunctionLValue (SimpleLValue _) = mempty
extractFunctionLValue (ArrayLValue _ v) = extractFunctionValue v

extractFunctionValue :: Value -> Set String
extractFunctionValue (StMonoOp _ v) = extractFunctionValue v
extractFunctionValue (StBinaryOp _ v1 v2) = extractFunctionValue v1 <> extractFunctionValue v2
extractFunctionValue (StLValue lv) = extractFunctionLValue lv
extractFunctionValue (StFunc s vs) = Set.singleton s <> foldMap extractFunctionValue vs
extractFunctionValue (StSubValue v) = extractFunctionValue v
extractFunctionValue _ = mempty
