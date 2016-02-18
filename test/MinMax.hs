module MinMax where

import BasePrelude
import Iec61499Types
import StTypes

minMaxBlock :: [BasicFunctionBlock]
minMaxBlock =
    [ BasicFunctionBlock
      { bfbDescription = FunctionBlockDescription "minMax" "Basic Function Block Type" "Loytec.HVAC"
      , bfbInterfaceList = InterfaceList
        { eventInputs = [ Event
                          { eventName = "REQ"
                          , eventComment = "Normal Execution Request"
                          , eventVariables = ["in1", "in2"]
                          }]
        , eventOutputs = [ Event
                           { eventName = "CNF"
                           , eventComment = "Execution Confirmation"
                           , eventVariables = ["minOut", "maxOut", "avgOut"]
                           }]
        , inputVariables = [ Variable
                             { variableName = "in1"
                             , variableType = IECReal
                             , variableComment = "Input event qualifier"
                             }
                           , Variable
                             { variableName = "in2"
                             , variableType = IECReal
                             , variableComment = ""
                             }]
        , outputVariables = [ Variable
                              { variableName = "minOut"
                              , variableType = IECReal
                              , variableComment = "Output event qualifier"
                              }
                            , Variable
                              { variableName = "maxOut"
                              , variableType = IECReal
                              , variableComment = ""
                              }
                            , Variable
                              { variableName = "avgOut"
                              , variableType = IECReal
                              , variableComment = ""
                              }]
        }
      , bfbVariables = []
      , bfbStates = [ ECState
                      { ecStateName = "START"
                      , ecStateComment = "Initial State"
                      , ecStateActions = []
                      , ecStatePosition = 552.9412 :+ 429.4117
                      }
                    , ECState
                      { ecStateName = "REQ"
                      , ecStateComment = "Normal execution"
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = "REQ"
                                           , ecActionOutput = "CNF"
                                           }]
                      , ecStatePosition = 217.647 :+ 752.9412
                      }]
      , bfbTransitions = [ ECTransition
                           { ecTransitionSource = "START"
                           , ecTransitionDestination = "REQ"
                           , ecTransitionCondition = StLValue
                                 (SimpleLValue "REQ")
                           , ecTransitionPosition = 447.8488 :+ 648.3352
                           }
                         , ECTransition
                           { ecTransitionSource = "REQ"
                           , ecTransitionDestination = "START"
                           , ecTransitionCondition = StInt 1
                           , ecTransitionPosition = 372.8767 :+ 512.7991
                           }]
      , bfbAlgorithms = [ ECAlgorithm
                          { ecAlgorithmName = "REQ"
                          , ecAlgorithmComment = "Normally executed algorithm"
                          , ecAlgorithmStText = [ Assignment
                                                      (SimpleLValue "minOut")
                                                      (StFunc
                                                           "MIN"
                                                           [ StLValue
                                                                 (SimpleLValue
                                                                      "in1")
                                                           , StLValue
                                                                 (SimpleLValue
                                                                      "in2")])
                                                , Assignment
                                                      (SimpleLValue "maxOut")
                                                      (StFunc
                                                           "MAX"
                                                           [ StLValue
                                                                 (SimpleLValue
                                                                      "in1")
                                                           , StLValue
                                                                 (SimpleLValue
                                                                      "in2")])
                                                , Assignment
                                                      (SimpleLValue "avgOut")
                                                      (StBinaryOp
                                                           StDivide
                                                           (StSubValue
                                                                (StBinaryOp
                                                                     StAddition
                                                                     (StLValue
                                                                          (SimpleLValue
                                                                               "in1"))
                                                                     (StLValue
                                                                          (SimpleLValue
                                                                               "in2"))))
                                                           (StInt 2))]
                          }]
      }]
