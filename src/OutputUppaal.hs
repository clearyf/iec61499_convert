module OutputUppaal where

import           BasePrelude
import           Control.Monad.Trans.State.Lazy (State, evalState, get, put)
import           Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as Map
import           ParseIec61499
import           ParseSt
import           Text.XML.HXT.Core

data UppaalModel = UppaalModel { modelName :: String
                               , modelInputEvents :: [UppaalChan]
                               , modelOutputEvents :: [UppaalChan]
                               , modelInputVars :: [UppaalVar]
                               , modelOutputVars :: [UppaalVar]
                               , modelLocations :: [Location]
                               , modelTransitions :: [Transition]
                               } deriving (Show,Eq)

data UppaalVar = UppaalVar String String deriving (Show,Eq)
newtype UppaalChan = UppaalChan String deriving (Show,Eq)

fbToUppaalModel :: FunctionBlock -> UppaalModel
fbToUppaalModel fb =
  UppaalModel (fbName fb)
              (inputChannels fb)
              (outputChannels fb)
              (inputParameters fb)
              (outputParameters fb)
              (locations fb)
              []

--------------------------------------------------------------------------------
-- 3. Handle events

extractChannels :: (InterfaceList -> [Event]) -> FunctionBlock -> [UppaalChan]
extractChannels f = map (UppaalChan . eventName) . f .interfaceList

inputChannels :: FunctionBlock -> [UppaalChan]
inputChannels = extractChannels eventInputs

outputChannels :: FunctionBlock -> [UppaalChan]
outputChannels = extractChannels eventOutputs

--------------------------------------------------------------------------------
-- 2. Handle variables

convertVariableType :: IECVariable -> String
convertVariableType IECBool = "bool"
convertVariableType IECInt = "int"
convertVariableType IECReal = error "Uppaal doesn't support Real types!"

inputParameters :: FunctionBlock -> [UppaalVar]
inputParameters = map createUppaalVar . inputVariables . interfaceList

outputParameters :: FunctionBlock -> [UppaalVar]
outputParameters = map createUppaalVar . outputVariables . interfaceList

createUppaalVar :: Variable -> UppaalVar
createUppaalVar var =
  UppaalVar (convertVariableType (variableType var)) (variableName var)

--------------------------------------------------------------------------------
-- 3. Handle Locations
--
-- IEC61499 associates the actions with the destination state, whereas
-- Uppaal associates the actions with the transition.  As well as
-- that, each transition in Uppaal can either wait on a channel or
-- send on a channel, so each action in IEC61499 must become an
-- additional urgent state in Uppaal.

newtype PriState = PriState String deriving (Show,Eq)
newtype SecState = SecState String deriving (Show,Eq)
newtype StateId  = StateId  String deriving (Show,Eq)

data Location = Location AState
              | UrgentLocation AState
              deriving (Show,Eq)

-- For each output event in an action a secondary location is
-- required.

locationStartPrefix :: ECState -> String
locationStartPrefix state = "__start_" <> ecStateName state

locationEventPrefix :: ECAction -> String
locationEventPrefix action = "__" <> ecActionOutput action <> "_" <> ecActionAlgorithm action <> "_"

locations :: FunctionBlock -> [Location]
-- locations fb = foldr f [] (zip  (map getLocationsFromState basicStates))
--   where f (i, (endState,urgentStates)) acc = acc
--         basicStates = getBasicStates fb
locations fb = doFold states
  where states = getStatesMap (getBasicStates fb)
        doFold (StateMap map) = Map.foldr f [] map
        f (urgentStates,normalState) acc = (map UrgentLocation urgentStates) <> [Location normalState] <> acc

data AState =
  AState {stateName :: String
         ,stateId :: StateId}
  deriving (Show,Eq)

newtype StateMap =
  StateMap (Map String ([AState],AState))
  deriving (Show,Eq)

getNextId :: State Int StateId
getNextId =
  do num <- get
     put (num + 1)
     pure (StateId ("id" <> (show num)))

initialStateId :: String
initialStateId = "id0"

getStatesMap :: [ECState] -> StateMap
getStatesMap basicStates =
  StateMap (Map.fromList
              (zip (map ecStateName basicStates)
                   (evalState (mapM getLocationsFromState basicStates) 0)))

getLocationsFromState :: ECState -> State Int ([AState],AState)
getLocationsFromState state =
  do initState <-
       if null (ecStateActions state)
          then pure []
          else fmap (: []) (createState locationStartPrefix state)
     actionStates <-
       mapM (createState locationEventPrefix)
            (ecStateActions state)
     destState <- createState ecStateName state
     pure (initState <> actionStates,destState)
  where createState f x =
          do nextId <- getNextId
             pure (AState (f x) nextId)

getBasicStates :: FunctionBlock -> [ECState]
getBasicStates = bfbStates . basicFb

--------------------------------------------------------------------------------

-- This file is divided into three sections, as the uppaal xml file is
-- basically subdivided into three sections, the global declarations,
-- the template declarations (locations & transitions) and finally the
-- system declaration.

outputUppaal :: FunctionBlock -> IO [String]
outputUppaal fb =
  runX (root [] [mkelem "nta" [] [sections]] >>>
        writeDocumentToString [withIndent yes])
  where sections = globalDecl fb <+> templateDecl fb <+> systemDecl fb

--------------------------------------------------------------------------------
-- The global declaration first.


createDecl = undefined

 -- createDecl :: FunctionBlock -> String
-- createDecl fb =
--   "// Global declarations\n" <>
--   mconcat (map (<> ";\n")
--                (inputChannels fb <> outputChannels fb <> inputParameters fb <>
--                 outputParameters fb))

-- The global declarations consist of the input/output events and
-- input/output values.
globalDecl :: ArrowXml a  => FunctionBlock -> a n XmlTree
globalDecl fb =
  selem "declarations" [txt (createDecl fb)]

--------------------------------------------------------------------------------
-- The template declaration second.

data Transition = Transition String deriving (Show,Eq)

-- TODO Fix coordinates for name declaration.
templateDecl :: ArrowXml a => FunctionBlock -> a n XmlTree
templateDecl fb =
  selem "template"
        ([mkelem "name"
                 [sattr "x" "0",sattr "y" "0"]
                 [txt (fbName fb)]
         ,selem "declarations" [txt "// Declarations\n"]] <>
         (makeLocations fb) <>
         (makeInitialLocation fb) <>
         (makeTransitions fb))

-- The locations all have a id reference associated with them.  These
-- start at id0.  All states which have output actions associated with
-- them require an additional "urgent" state to run the relevant
-- algorithm and output the event (ie do "e!" in the sync block).


-- -- Zip this function with ids to get the ids <-> states mapping.
-- getAllLocations :: FunctionBlock -> [Location]
-- getAllLocations fb =
--   foldr f [] (getBasicStates fb)
--   where f state lst =
--           (entryState state) :
--           (urgentActions state) <> [Location (ecStateName state)] <> lst
--         entryState state =
--           UrgentLocation (ecStateName state <> "_entryState")
--         urgentActions state =
--           map (\action ->
--                  UrgentLocation
--                    (ecStateName state <> "_" <>
--                     ecActionAlgorithm action <> "_" <>
--                     ecActionOutput action))
--               (ecStateActions state)

getAllStates = undefined

-- getAllStates :: FunctionBlock -> [String]
-- getAllStates fb = map f (getAllLocations fb)
--   where f (Location s) = s
--         f (UrgentLocation s) = s

makeLocations = undefined

-- makeLocations :: ArrowXml a => FunctionBlock -> [a n XmlTree]
-- makeLocations fb = zipWith makeLocation (getAllLocations fb) ids

makeLocation = undefined

-- -- TOOD Fix coordinates.
-- makeLocation :: ArrowXml a => Location -> String -> a n XmlTree
-- makeLocation (Location name) thisid =
--   mkelem "location"
--          [sattr "id" thisid,sattr "x" "0",sattr "y" "0"]
--          [selem "name" [txt name]]
-- makeLocation (UrgentLocation name) thisid =
--   mkelem "location"
--          [sattr "id" thisid,sattr "x" "0",sattr "y" "0"]
--          [selem "name" [txt name],eelem "urgent"]

-- TODO it's not quite clear how the initial state of the system is
-- defined; either it's the first state listed in the structure, or
-- it's the one called "START".  Take the first state...
makeInitialLocation :: ArrowXml a => FunctionBlock -> [a n XmlTree]
makeInitialLocation _ = [aelem "init" [sattr "ref" "id0"]]

getTransitions :: FunctionBlock -> [ECTransition]
getTransitions = bfbTransitions . basicFb

getTransitionSrcDst :: FunctionBlock -> [(String,String)]
getTransitionSrcDst fb =
  map (\tr ->
         (ecTransitionSource tr,ecTransitionDestination tr))
      (getTransitions fb)

-- Generate the synchronisation action for a destination state.
-- IEC61499 has the transition events associated with the destination
-- state, whereas in Uppaal the transitions and not the locations (the
-- states) have the events.  TODO Don't use fromJust.
getSync :: FunctionBlock -> String -> String
getSync fb dst = foldMap (<>"!") outputs
  where
    state = fromJust (find (\ x -> ecStateName x == dst) (getBasicStates fb))
    -- For each state create a list of output channels to fire.
    outputs = nub (map ecActionOutput (ecStateActions state))

-- Get the synchronisation condition for the source state.
getSourceTransitions :: String -> String -> [Transition]
getSourceTransitions = undefined

makeTransitions = undefined

-- -- TODO Fix coordinates.
-- makeTransitions :: ArrowXml a => FunctionBlock -> [a n XmlTree]
-- makeTransitions fb =
--   map makeTransition (getTransitionSrcDst fb)
--   where makeTransition (src,dst) =
--           selem "transition"
--                 [aelem "source" [sattr "ref" (refMap ! src)]
--                 ,aelem "target" [sattr "ref" (refMap ! dst)]
--                 ,mkelem "label"
--                         [sattr "kind" "synchronisation"
--                         ,sattr "x" "0"
--                         ,sattr "y" "0"]
--                         [txt (getSync fb dst)]
--                 ,mkelem "label"
--                         [sattr "kind" "assignment"
--                         ,sattr "x" "0"
--                         ,sattr "y" "0"]
--                         [txt "TODO"]
--                 ,aelem "nail" [sattr "x" "0",sattr "y" "0"]]
--         refMap =
--           Map.fromList (zip (getAllStates fb) ids)

--------------------------------------------------------------------------------
-- The system declaration last.

createSystem :: FunctionBlock -> String
createSystem fb = "// System setup\n" <> systemName <> "blk = " <> systemName <>
                  "();\nsystem " <> systemName <> "blk;\n"
  where systemName = fbName fb

systemDecl :: ArrowXml a => FunctionBlock -> a n XmlTree
systemDecl fb = selem "system" [txt (createSystem fb)]

--------------------------------------------------------------------------------
-- Temp Testing Stuff.

test :: IO ()
test = do output <- outputUppaal tfb
          traverse_ putStr output

tfb :: FunctionBlock
tfb =
  FunctionBlock {fbName = "Toggle"
                ,interfaceList =
                   InterfaceList {eventInputs =
                                    [Event {eventName = "Toggle"
                                           ,eventComment = "Normal Execution Request"
                                           ,eventVariables = []}]
                                 ,eventOutputs =
                                    [Event {eventName = "Update"
                                           ,eventComment = "Execution Confirmation"
                                           ,eventVariables =
                                              ["Value"]}]
                                 ,inputVariables = []
                                 ,outputVariables =
                                    [Variable {variableName = "Value"
                                              ,variableType = IECBool
                                              ,variableComment = "Output event qualifier"}]}
                ,basicFb =
                   BasicFunctionBlock {bfbStates =
                                         [ECState {ecStateName = "START"
                                                  ,ecStateComment = "Initial AState"
                                                  ,ecStateActions = []}
                                         ,ECState {ecStateName = "On"
                                                  ,ecStateComment = "Normal execution"
                                                  ,ecStateActions =
                                                     [ECAction {ecActionAlgorithm = "TurnOn"
                                                               ,ecActionOutput = "Update"}]}
                                         ,ECState {ecStateName = "Off"
                                                  ,ecStateComment = ""
                                                  ,ecStateActions =
                                                     [ECAction {ecActionAlgorithm = "TurnOff"
                                                               ,ecActionOutput = "Update"}]}]
                                      ,bfbTransitions =
                                         [ECTransition {ecTransitionSource = "START"
                                                       ,ecTransitionDestination = "Off"
                                                       ,ecTransitionCondition = "1"}
                                         ,ECTransition {ecTransitionSource = "Off"
                                                       ,ecTransitionDestination = "On"
                                                       ,ecTransitionCondition = "Toggle"}
                                         ,ECTransition {ecTransitionSource = "On"
                                                       ,ecTransitionDestination = "Off"
                                                       ,ecTransitionCondition = "Toggle"}]
                                      ,bfbAlgorithms =
                                         [ECAlgorithm {ecAlgorithmName = "TurnOn"
                                                      ,ecAlgorithmComment = "Normally executed algorithm"
                                                      ,ecAlgorithmStText =
                                                         [Assignment "Value" "TRUE"]}
                                         ,ECAlgorithm {ecAlgorithmName = "TurnOff"
                                                      ,ecAlgorithmComment = ""
                                                      ,ecAlgorithmStText =
                                                         [Assignment "Value" "FALSE"]}]}}
