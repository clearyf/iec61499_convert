module MinMax where

import BasePrelude
import ParseIec61499
import ParseSt

minMaxBlock :: [BasicFunctionBlock]
minMaxBlock =
    [ BasicFunctionBlock
      { bfbName = "minMax"
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
                      , ecStatePosition = (-305.01816) :+ 461.2027
                      }
                    , ECState
                      { ecStateName = "REQ"
                      , ecStateComment = "Normal execution"
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = "REQ"
                                           , ecActionOutput = "CNF"
                                           }]
                      , ecStatePosition = 109.98699 :+ (-187.8113)
                      }]
      , bfbTransitions = [ ECTransition
                           { ecTransitionSource = "START"
                           , ecTransitionDestination = "REQ"
                           , ecTransitionCondition = StLValue
                                 (SimpleLValue "REQ")
                           , ecTransitionPosition = 175.91646 :+ 411.85184
                           }
                         , ECTransition
                           { ecTransitionSource = "REQ"
                           , ecTransitionDestination = "START"
                           , ecTransitionCondition = StInt 1
                           , ecTransitionPosition = (-280.46118) :+ (-245.7205)
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
