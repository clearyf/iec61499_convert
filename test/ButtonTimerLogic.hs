module ButtonTimerLogic where

import BasePrelude
import ParseIec61499
import ParseSt

buttonTimerLogicBlock :: [BasicFunctionBlock]
buttonTimerLogicBlock =
    [ BasicFunctionBlock
      { bfbDescription = FunctionBlockDescription "ButtonTimerLogic" "Basic Function Block Type" "Loytec.Automation"
      , bfbInterfaceList = InterfaceList
        { eventInputs = [ Event
                          { eventName = "BUTTON"
                          , eventComment = "Button event"
                          , eventVariables = ["button", "longSeconds"]
                          }
                        , Event
                          { eventName = "DELAY_DONE"
                          , eventComment = "External delay event"
                          , eventVariables = ["button"]
                          }]
        , eventOutputs = [ Event
                           { eventName = "DELAY_START"
                           , eventComment = "Start external delay"
                           , eventVariables = ["delayTime"]
                           }
                         , Event
                           { eventName = "DELAY_STOP"
                           , eventComment = "Stop external delay"
                           , eventVariables = []
                           }
                         , Event
                           { eventName = "PRESSED"
                           , eventComment = "Event when button is pressed"
                           , eventVariables = []
                           }
                         , Event
                           { eventName = "RELEASED"
                           , eventComment = "Event when button is released"
                           , eventVariables = []
                           }
                         , Event
                           { eventName = "SHORT"
                           , eventComment = "Event when short press is detected"
                           , eventVariables = []
                           }
                         , Event
                           { eventName = "LONG"
                           , eventComment = "Event when long press is detected"
                           , eventVariables = []
                           }]
        , inputVariables = [ Variable
                             { variableName = "button"
                             , variableType = IECBool
                             , variableComment = "Button input"
                             }
                           , Variable
                             { variableName = "longSeconds"
                             , variableType = IECInt Sixteen
                             , variableComment = "Time for long press"
                             }]
        , outputVariables = [ Variable
                              { variableName = "delayTime"
                              , variableType = IECInt Sixteen
                              , variableComment = "Time for external delay"
                              }]
        }
      , bfbVariables = []
      , bfbStates = [ ECState
                      { ecStateName = "START"
                      , ecStateComment = "Initial State"
                      , ecStateActions = []
                      , ecStatePosition = 1344.941 :+ 109.4117
                      }
                    , ECState
                      { ecStateName = "DOWN"
                      , ecStateComment = ""
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = "Down"
                                           , ecActionOutput = "DELAY_START"
                                           }
                                         , ECAction
                                           { ecActionAlgorithm = ""
                                           , ecActionOutput = "PRESSED"
                                           }]
                      , ecStatePosition = 1412.0 :+ 588.0
                      }
                    , ECState
                      { ecStateName = "UP"
                      , ecStateComment = ""
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = ""
                                           , ecActionOutput = "RELEASED"
                                           }]
                      , ecStatePosition = 1352.0 :+ 1760.0
                      }
                    , ECState
                      { ecStateName = "SHORT"
                      , ecStateComment = ""
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = ""
                                           , ecActionOutput = "SHORT"
                                           }
                                         , ECAction
                                           { ecActionAlgorithm = ""
                                           , ecActionOutput = "DELAY_STOP"
                                           }]
                      , ecStatePosition = 952.0 :+ 1244.0
                      }
                    , ECState
                      { ecStateName = "LONG"
                      , ecStateComment = ""
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = ""
                                           , ecActionOutput = "LONG"
                                           }]
                      , ecStatePosition = 1904.0 :+ 1236.0
                      }]
      , bfbTransitions = [ ECTransition
                           { ecTransitionSource = "START"
                           , ecTransitionDestination = "DOWN"
                           , ecTransitionCondition = StBinaryOp
                                 StAnd
                                 (StLValue (SimpleLValue "BUTTON"))
                                 (StLValue (SimpleLValue "button"))
                           , ecTransitionPosition = 1452.703 :+ 339.4936
                           }
                         , ECTransition
                           { ecTransitionSource = "DOWN"
                           , ecTransitionDestination = "SHORT"
                           , ecTransitionCondition = StBinaryOp
                                 StAnd
                                 (StLValue (SimpleLValue "BUTTON"))
                                 (StMonoOp
                                      StNot
                                      (StLValue (SimpleLValue "button")))
                           , ecTransitionPosition = 1090.045 :+ 858.4637
                           }
                         , ECTransition
                           { ecTransitionSource = "UP"
                           , ecTransitionDestination = "START"
                           , ecTransitionCondition = StInt 1
                           , ecTransitionPosition = 425.8619 :+ 870.0295
                           }
                         , ECTransition
                           { ecTransitionSource = "DOWN"
                           , ecTransitionDestination = "LONG"
                           , ecTransitionCondition = StLValue
                                 (SimpleLValue "DELAY_DONE")
                           , ecTransitionPosition = 1734.34 :+ 885.2359
                           }
                         , ECTransition
                           { ecTransitionSource = "LONG"
                           , ecTransitionDestination = "UP"
                           , ecTransitionCondition = StBinaryOp
                                 StAnd
                                 (StLValue (SimpleLValue "BUTTON"))
                                 (StMonoOp
                                      StNot
                                      (StLValue (SimpleLValue "button")))
                           , ecTransitionPosition = 1707.58 :+ 1501.754
                           }
                         , ECTransition
                           { ecTransitionSource = "SHORT"
                           , ecTransitionDestination = "UP"
                           , ecTransitionCondition = StInt 1
                           , ecTransitionPosition = 1186.319 :+ 1543.395
                           }]
      , bfbAlgorithms = [ ECAlgorithm
                          { ecAlgorithmName = "Down"
                          , ecAlgorithmComment = ""
                          , ecAlgorithmStText = [ Assignment
                                                      (SimpleLValue "delayTime")
                                                      (StFunc
                                                           "REAL_TO_TIME"
                                                           [ StBinaryOp
                                                                 StMultiply
                                                                 (StLValue
                                                                      (SimpleLValue
                                                                           "longSeconds"))
                                                                 (StFloat
                                                                      1000.0)])]
                          }]
      }]
