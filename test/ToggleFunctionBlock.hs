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
        { bfbVariables = [ Variable { variableName = "Blah"
                                    , variableType = IECUInt Sixteen
                                    , variableComment = "Unused Variable"
                                    }]
        , bfbStates = [ ECState
                        { ecStateName = "START"
                        , ecStateComment = "Initial State"
                        , ecStateActions = []
                        , ecStatePosition = (-305.01816) :+ 461.2027
                        }
                      , ECState
                        { ecStateName = "On"
                        , ecStateComment = "Normal execution"
                        , ecStateActions = [ ECAction
                                             { ecActionAlgorithm = "TurnOn"
                                             , ecActionOutput = "Update"
                                             }]
                        , ecStatePosition = 109.98699 :+ (-187.8113)
                        }
                      , ECState
                        { ecStateName = "Off"
                        , ecStateComment = ""
                        , ecStateActions = [ ECAction
                                             { ecActionAlgorithm = "TurnOff"
                                             , ecActionOutput = "Update"
                                             }]
                        , ecStatePosition = 706.1995 :+ 894.9202
                        }]
        , bfbTransitions = [ ECTransition
                             { ecTransitionSource = "START"
                             , ecTransitionDestination = "Off"
                             , ecTransitionCondition = StInt 1
                             , ecTransitionPosition = (-770.4371) :+ 464.92926
                             }
                           , ECTransition
                             { ecTransitionSource = "Off"
                             , ecTransitionDestination = "On"
                             , ecTransitionCondition = StLValue (SimpleLValue "Toggle")
                             , ecTransitionPosition = (-404.7159) :+ 544.991
                             }
                           , ECTransition
                             { ecTransitionSource = "On"
                             , ecTransitionDestination = "Off"
                             , ecTransitionCondition = StLValue (SimpleLValue "Toggle")
                             , ecTransitionPosition = (-372.27936) :+ (-574.5198)
                             }]
        , bfbAlgorithms = [ ECAlgorithm
                            { ecAlgorithmName = "TurnOn"
                            , ecAlgorithmComment = "Normally executed algorithm"
                            , ecAlgorithmStText = [ Assignment
                                                        (SimpleLValue "Value")
                                                        (StBool True)]
                            }
                          , ECAlgorithm
                            { ecAlgorithmName = "TurnOff"
                            , ecAlgorithmComment = ""
                            , ecAlgorithmStText = [ Assignment
                                                        (SimpleLValue "Value")
                                                        (StBool False)]
                            }]
        }
      }]
