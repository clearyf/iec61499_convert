module Main where

import BasePrelude
import Test.Hspec (hspec, describe, it, shouldBe, runIO)
import Test.Hspec.Core.Spec (Spec)
import ParseIec61499
import ParseSt
import FbToUppaal
import OutputUppaal

main :: IO ()
main = hspec tests

tests :: Spec
tests = do
    describe "Parse IEC61499" testParseIec61499
    describe "Parse ST" testParseSt
    describe "Function Block to Uppaal Model" testUppaalModel
    describe "Uppaal Model to XML" testOutputUppaal

testParseIec61499 :: Spec
testParseIec61499 = do
    toggleFunctionBlockFromFile <- runIO (readFunctionBlock toggleFile)
    it
        "Function Block"
        (toggleFunctionBlockFromFile `shouldBe` toggleFunctionBlock)

testParseSt :: Spec
testParseSt = do
    it
        "Simple Assigment"
        (do parseSt "Value:=FALSE;" `shouldBe`
                Right [Assignment "Value" [StBool False]]
            parseSt "Value := -303;" `shouldBe`
                Right [Assignment "Value" [StInt (-303)]]
            parseSt "Value := blah;" `shouldBe`
                Right [Assignment "Value" [StVar "blah"]]
            parseSt "Value := TRUE;\ni := 3;\n" `shouldBe`
                Right
                    [Assignment "Value" [StBool True], Assignment "i" [StInt 3]])

testOutputUppaal :: Spec
testOutputUppaal = do
    outputModel <- runIO (outputUppaal uppaalModel)
    modelFromXml <- runIO (readFile "examples/uppaal/noheaderToggle.xml")
    it "toggle model" (outputModel `shouldBe` [modelFromXml])

toggleFile :: FilePath
toggleFile = "examples/iec61499/toggle_fb.xml"

toggleFunctionBlock :: [FunctionBlock]
toggleFunctionBlock =
    [ FunctionBlock
      { fbName = "Toggle"
      , interfaceList = InterfaceList
        { eventInputs = [ Event
                          { eventName = "Toggle"
                          , eventComment = "Normal Execution Request"
                          , eventVariables = []
                          }]
        , eventOutputs = [ Event
                           { eventName = "Update"
                           , eventComment = "Execution Confirmation"
                           , eventVariables = ["Value"]
                           }]
        , inputVariables = []
        , outputVariables = [ Variable
                              { variableName = "Value"
                              , variableType = IECBool
                              , variableComment = "Output event qualifier"
                              }]
        }
      , basicFb = BasicFunctionBlock
        { bfbStates = [ ECState
                        { ecStateName = "START"
                        , ecStateComment = "Initial State"
                        , ecStateActions = []
                        }
                      , ECState
                        { ecStateName = "On"
                        , ecStateComment = "Normal execution"
                        , ecStateActions = [ ECAction
                                             { ecActionAlgorithm = "TurnOn"
                                             , ecActionOutput = "Update"
                                             }]
                        }
                      , ECState
                        { ecStateName = "Off"
                        , ecStateComment = ""
                        , ecStateActions = [ ECAction
                                             { ecActionAlgorithm = "TurnOff"
                                             , ecActionOutput = "Update"
                                             }]
                        }]
        , bfbTransitions = [ ECTransition
                             { ecTransitionSource = "START"
                             , ecTransitionDestination = "Off"
                             , ecTransitionCondition = "1"
                             }
                           , ECTransition
                             { ecTransitionSource = "Off"
                             , ecTransitionDestination = "On"
                             , ecTransitionCondition = "Toggle"
                             }
                           , ECTransition
                             { ecTransitionSource = "On"
                             , ecTransitionDestination = "Off"
                             , ecTransitionCondition = "Toggle"
                             }]
        , bfbAlgorithms = [ ECAlgorithm
                            { ecAlgorithmName = "TurnOn"
                            , ecAlgorithmComment = "Normally executed algorithm"
                            , ecAlgorithmStText = [Assignment "Value" [StBool True]]
                            }
                          , ECAlgorithm
                            { ecAlgorithmName = "TurnOff"
                            , ecAlgorithmComment = ""
                            , ecAlgorithmStText = [Assignment "Value" [StBool False]]
                            }]
        }
      }]

testUppaalModel :: Spec
testUppaalModel = do
    it
        "Toggle system"
        ((fmap fbToUppaalModel toggleFunctionBlock) `shouldBe` [uppaalModel])

uppaalModel :: UppaalModel
uppaalModel =
    UppaalModel
    { modelName = "Toggle"
    , modelInputEvents = [UppaalChan "ic_Toggle"]
    , modelOutputEvents = [UppaalChan "oc_Update"]
    , modelInputVars = []
    , modelOutputVars = [UppaalVar "bool" "Value"]
    , modelLocations = [ UrgentLocation
                             (AState
                              { stateName = "__start_Off"
                              , stateId = StateId "id4"
                              })
                       , UrgentLocation
                             (AState
                              { stateName = "__action_Off__Update_TurnOff_"
                              , stateId = StateId "id5"
                              })
                       , Location
                             (AState
                              { stateName = "Off"
                              , stateId = StateId "id6"
                              })
                       , UrgentLocation
                             (AState
                              { stateName = "__start_On"
                              , stateId = StateId "id1"
                              })
                       , UrgentLocation
                             (AState
                              { stateName = "__action_On__Update_TurnOn_"
                              , stateId = StateId "id2"
                              })
                       , Location
                             (AState
                              { stateName = "On"
                              , stateId = StateId "id3"
                              })
                       , Location
                             (AState
                              { stateName = "START"
                              , stateId = StateId "id0"
                              })]
    , modelTransitions = [ Transition
                           { transitionSrc = StateId "id0"
                           , transitionDest = StateId "id4"
                           , transitionSync = ""
                           , transitionUpdate = ""
                           }
                         , Transition
                           { transitionSrc = StateId "id6"
                           , transitionDest = StateId "id1"
                           , transitionSync = "ic_Toggle?"
                           , transitionUpdate = ""
                           }
                         , Transition
                           { transitionSrc = StateId "id3"
                           , transitionDest = StateId "id4"
                           , transitionSync = "ic_Toggle?"
                           , transitionUpdate = ""
                           }
                         , Transition
                           { transitionSrc = StateId "id1"
                           , transitionDest = StateId "id2"
                           , transitionSync = "oc_Update!"
                           , transitionUpdate = "TurnOn();"
                           }
                         , Transition
                           { transitionSrc = StateId "id2"
                           , transitionDest = StateId "id3"
                           , transitionSync = ""
                           , transitionUpdate = ""
                           }
                         , Transition
                           { transitionSrc = StateId "id4"
                           , transitionDest = StateId "id5"
                           , transitionSync = "oc_Update!"
                           , transitionUpdate = "TurnOff();"
                           }
                         , Transition
                           { transitionSrc = StateId "id5"
                           , transitionDest = StateId "id6"
                           , transitionSync = ""
                           , transitionUpdate = ""
                           }]
    , modelDeclarations = [ "void TurnOn()\n{\n\tValue = true;\n}\n"
                          , "void TurnOff()\n{\n\tValue = false;\n}\n"]
    }
