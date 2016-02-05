module ToggleFunctionBlock where

import BasePrelude
import ParseIec61499
import ParseSt

toggleFunctionBlock :: [BasicFunctionBlock]
toggleFunctionBlock =
    [ BasicFunctionBlock
      { bfbDescription = FunctionBlockDescription "Toggle" "Basic Function Block Type" "Main"
      , bfbInterfaceList = InterfaceList
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
      , bfbVariables = [ Variable
                         { variableName = "Blah"
                         , variableType = IECUInt Sixteen
                         , variableComment = "Unused Variable"
                         }]
      , bfbStates = [ ECState
                      { ecStateName = "START"
                      , ecStateComment = "Initial State"
                      , ecStateActions = []
                      , ecStatePosition = 552.9412 :+ 429.4117
                      }
                    , ECState
                      { ecStateName = "On"
                      , ecStateComment = "Normal execution"
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = "TurnOn"
                                           , ecActionOutput = "Update"
                                           }]
                      , ecStatePosition = 217.647 :+ 752.9412
                      }
                    , ECState
                      { ecStateName = "Off"
                      , ecStateComment = ""
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = "TurnOff"
                                           , ecActionOutput = "Update"
                                           }]
                      , ecStatePosition = 1140.0 :+ 824.0
                      }]
      , bfbTransitions = [ ECTransition
                           { ecTransitionSource = "START"
                           , ecTransitionDestination = "Off"
                           , ecTransitionCondition = StInt 1
                           , ecTransitionPosition = 899.8514 :+ 561.8021
                           }
                         , ECTransition
                           { ecTransitionSource = "Off"
                           , ecTransitionDestination = "On"
                           , ecTransitionCondition = StLValue
                                 (SimpleLValue "Toggle")
                           , ecTransitionPosition = 678.83 :+ 869.2891
                           }
                         , ECTransition
                           { ecTransitionSource = "On"
                           , ecTransitionDestination = "Off"
                           , ecTransitionCondition = StLValue
                                 (SimpleLValue "Toggle")
                           , ecTransitionPosition = 684.591 :+ 707.8542
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
      }]
