module ToggleFunctionBlock where

import BasePrelude
import ParseIec61499
import ParseSt

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
                            , ecAlgorithmStText = [ Assignment
                                                        (SimpleLValue "Value")
                                                        [StBool True]]
                            }
                          , ECAlgorithm
                            { ecAlgorithmName = "TurnOff"
                            , ecAlgorithmComment = ""
                            , ecAlgorithmStText = [ Assignment
                                                        (SimpleLValue "Value")
                                                        [StBool False]]
                            }]
        }
      }]
