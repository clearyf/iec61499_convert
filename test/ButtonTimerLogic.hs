module ButtonTimerLogic where

import BasePrelude
import ParseIec61499
import ParseSt

buttonTimerLogicBlock :: [BasicFunctionBlock]
buttonTimerLogicBlock =
    [ BasicFunctionBlock
      { fbName = "ButtonTimerLogic"
      , interfaceList = InterfaceList
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
                      , ecStatePosition = (-1150.7609) :+ 696.1437
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
                      , ecStatePosition = (-1223.8326) :+ (-704.2568)
                      }
                    , ECState
                      { ecStateName = "UP"
                      , ecStateComment = ""
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = ""
                                           , ecActionOutput = "RELEASED"
                                           }]
                      , ecStatePosition = 1026.9656 :+ 879.34393
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
                      , ecStatePosition = 949.6223 :+ (-67.24162)
                      }
                    , ECState
                      { ecStateName = "LONG"
                      , ecStateComment = ""
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = ""
                                           , ecActionOutput = "LONG"
                                           }]
                      , ecStatePosition = (-409.39227) :+ (-1859.4661)
                      }]
      , bfbTransitions = [ ECTransition
                           { ecTransitionSource = "START"
                           , ecTransitionDestination = "DOWN"
                           , ecTransitionCondition = StBinaryOp
                                 StAnd
                                 (StLValue (SimpleLValue "BUTTON"))
                                 (StLValue (SimpleLValue "button"))
                           , ecTransitionPosition = 1423.2865 :+ 290.8634
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
                           , ecTransitionPosition = (-752.4605) :+ (-788.67065)
                           }
                         , ECTransition
                           { ecTransitionSource = "UP"
                           , ecTransitionDestination = "START"
                           , ecTransitionCondition = StInt 1
                           , ecTransitionPosition = (-418.06207) :+ 81.132385
                           }
                         , ECTransition
                           { ecTransitionSource = "DOWN"
                           , ecTransitionDestination = "LONG"
                           , ecTransitionCondition = StLValue
                                 (SimpleLValue "DELAY_DONE")
                           , ecTransitionPosition = 1334.0328 :+ (-1108.2831)
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
                           , ecTransitionPosition = 1703.0645 :+ 124.0997
                           }
                         , ECTransition
                           { ecTransitionSource = "SHORT"
                           , ecTransitionDestination = "UP"
                           , ecTransitionCondition = StInt 1
                           , ecTransitionPosition = (-762.2221) :+ (-909.049)
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
