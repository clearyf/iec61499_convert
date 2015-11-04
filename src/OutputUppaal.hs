{-# LANGUAGE NoImplicitPrelude #-}
module OutputUppaal (outputUppaal,test) where

import           BasePrelude
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import           ParseIec61499
import           ParseSt
import           Text.XML.HXT.Core

inputChannels :: FunctionBlock -> [String]
inputChannels =
  map (("chan " <>) . eventName) . eventInputs . interfaceList

outputChannels :: FunctionBlock -> [String]
outputChannels =
  map (("chan " <>) . eventName) . eventOutputs . interfaceList

convertVariableType :: IECVariable -> String
convertVariableType IECBool = "bool"
convertVariableType IECInt = "int"
convertVariableType IECReal = error "Uppaal doesn't support Real types!"

inputParameters :: FunctionBlock -> [String]
inputParameters =
  map (parameterPrefix "in") . inputVariables . interfaceList

outputParameters :: FunctionBlock -> [String]
outputParameters =
  map (parameterPrefix "out") . outputVariables . interfaceList

parameterPrefix :: String -> Variable -> String
parameterPrefix prefix var =
  convertVariableType (variableType var) <> " " <> prefix <> variableName var

createDecl :: FunctionBlock -> String
createDecl fb =
  mconcat (map (<> ";\n")
               (inputChannels fb <> outputChannels fb <> inputParameters fb <>
                outputParameters fb))

-- The global declarations consist of the input/output events and
-- input/output values.
globalDeclarations :: ArrowXml a  => FunctionBlock -> a n XmlTree
globalDeclarations fb =
  selem "declarations" [txt (createDecl fb)]

-- TODO Fix coordinates for name declaration.
templateDeclarations :: ArrowXml a => FunctionBlock -> a n XmlTree
templateDeclarations fb =
  selem "template"
        ([mkelem "name"
                 [sattr "x" "0",sattr "y" "0"]
                 [txt (fbName fb)]
         ,eelem "declarations"] <>
         (getLocations fb) <>
         (insertInitialLocation fb) <>
         (insertTransitions fb))

getStates :: FunctionBlock -> [ECState]
getStates fb =
  catMaybes (map (extractState)
                 (bfbElements (basicFb fb)))

getStateNames :: FunctionBlock -> [String]
getStateNames fb = map ecStateName (getStates fb)

extractState :: MonadPlus m => ECCElement -> m ECState
extractState (ECCState state) = return state
extractState _ = mzero

ids :: [String]
ids = map (("id"<>) . show) ([0..]::[Int])

-- The locations all have a id reference associated with them.  These
-- start at id0.
getLocations :: ArrowXml a => FunctionBlock -> [a n XmlTree]
getLocations fb = zipWith makeLocation (getStateNames fb) ids

-- TOOD Fix coordinates.
makeLocation :: ArrowXml a => String -> String -> a n XmlTree
makeLocation name thisid =
  mkelem "location"
         [sattr "id" thisid,sattr "x" "0",sattr "y" "0"]
         [selem "name" [txt name]]

-- TOOD Fix coordinates.
makeUrgentLocation :: ArrowXml a => String -> String -> a n XmlTree
makeUrgentLocation name thisid =
  mkelem "location"
         [sattr "id" thisid,sattr "x" "0",sattr "y" "0"]
         [selem "name" [txt name],eelem "urgent"]

-- TODO it's not quite clear how the initial state of the system is
-- defined; either it's the first state listed in the structure, or
-- it's the one called "START".  Take the first state...
insertInitialLocation :: ArrowXml a => FunctionBlock -> [a n XmlTree]
insertInitialLocation _ = [aelem "init" [sattr "ref" "id0"]]

getTransitions :: FunctionBlock -> [ECTransition]
getTransitions fb =
  catMaybes (map extractTransition (bfbElements (basicFb fb)))

getTransitionSrcDst :: FunctionBlock -> [(String,String)]
getTransitionSrcDst fb =
  map (\tr ->
         (ecTransitionSource tr,ecTransitionDestination tr))
      (getTransitions fb)

extractTransition :: MonadPlus m => ECCElement -> m ECTransition
extractTransition (ECCTransition tr) = return tr
extractTransition _ = mzero

-- Generate the synchronisation action for a destination state.
-- IEC61499 has the transition events associated with the destination
-- state, whereas in Uppaal the transitions and not the locations (the
-- states) have the events.  TODO Don't use fromJust.
getSync :: FunctionBlock -> String -> String
getSync fb dst = mconcat (map (<>"!\n") outputs)
  where
    state = fromJust (find (\ x -> ecStateName x == dst) (getStates fb))
    -- For each state create a list of output channels to fire.
    outputs = nub (map ecActionOutput (ecStateActions state))

-- TODO Fix coordinates.
insertTransitions :: ArrowXml a => FunctionBlock -> [a n XmlTree]
insertTransitions fb =
  map makeTransition (getTransitionSrcDst fb)
  where makeTransition (src,dst) =
          selem "transition"
                [aelem "source" [sattr "ref" (refMap ! src)]
                ,aelem "target" [sattr "ref" (refMap ! dst)]
                ,mkelem "label"
                        [sattr "kind" "synchronisation"
                        ,sattr "x" "0"
                        ,sattr "y" "0"]
                        [txt (getSync fb dst)]
                ,mkelem "label"
                        [sattr "kind" "assignment"
                        ,sattr "x" "0"
                        ,sattr "y" "0"]
                        [txt "TODO"]
                ,aelem "nail" [sattr "x" "0",sattr "y" "0"]]
        refMap =
          Map.fromList (zip (getStateNames fb) ids)

createSystem :: FunctionBlock -> String
createSystem fb = systemName <> "blk = " <> systemName <> "();\nsystem " <>
                  systemName <> "blk;\n"
  where systemName = fbName fb

systemDeclarations :: ArrowXml a => FunctionBlock -> a n XmlTree
systemDeclarations fb = selem "system" [txt (createSystem fb)]

outputUppaal :: FunctionBlock -> IO [String]
outputUppaal fb =
  runX (root [] [mkelem "nta" [] [sections]] >>>
        writeDocumentToString [withIndent yes])
  where sections = globalDecl <+> templateSection <+> systemDecl
        globalDecl = globalDeclarations fb
        templateSection = templateDeclarations fb
        systemDecl = systemDeclarations fb

test :: IO ()
test = do output <- outputUppaal toggleFunctionBlock
          putStr (mconcat output)

toggleFunctionBlock :: FunctionBlock
toggleFunctionBlock =
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
                   BasicFunctionBlock {bfbElements =
                                         [ECCState (ECState {ecStateName = "START"
                                                            ,ecStateComment = "Initial State"
                                                            ,ecStateActions = []})
                                         ,ECCState (ECState {ecStateName = "On"
                                                            ,ecStateComment = "Normal execution"
                                                            ,ecStateActions =
                                                               [ECAction {ecActionAlgorithm = "TurnOn"
                                                                         ,ecActionOutput = "Update"}]})
                                         ,ECCState (ECState {ecStateName = "Off"
                                                            ,ecStateComment = ""
                                                            ,ecStateActions =
                                                               [ECAction {ecActionAlgorithm = "TurnOff"
                                                                         ,ecActionOutput = "Update"}]})
                                         ,ECCTransition
                                            (ECTransition {ecTransitionSource = "START"
                                                          ,ecTransitionDestination = "Off"
                                                          ,ecTransitionCondition = "1"})
                                         ,ECCTransition
                                            (ECTransition {ecTransitionSource = "Off"
                                                          ,ecTransitionDestination = "On"
                                                          ,ecTransitionCondition = "Toggle"})
                                         ,ECCTransition
                                            (ECTransition {ecTransitionSource = "On"
                                                          ,ecTransitionDestination = "Off"
                                                          ,ecTransitionCondition = "Toggle"})]
                                      ,bfbAlgorithms =
                                         [ECAlgorithm {ecAlgorithmName = "TurnOn"
                                                      ,ecAlgorithmComment = "Normally executed algorithm"
                                                      ,ecAlgorithmStText =
                                                         [Assignment "Value" "TRUE"]}
                                         ,ECAlgorithm {ecAlgorithmName = "TurnOff"
                                                      ,ecAlgorithmComment = ""
                                                      ,ecAlgorithmStText =
                                                         [Assignment "Value" "FALSE"]}]}}
