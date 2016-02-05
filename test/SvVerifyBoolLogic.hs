module SvVerifyBoolLogic where

import BasePrelude
import ParseIec61499
import ParseSt

svVerifyBoolLogicBlock :: [BasicFunctionBlock]
svVerifyBoolLogicBlock =
    [ BasicFunctionBlock
      { bfbDescription = FunctionBlockDescription "svVerifyBoolLogic" "Basic Function Block Type" "Loytec.Automation"
      , bfbInterfaceList = InterfaceList
        { eventInputs = [ Event
                          { eventName = "VALUE"
                          , eventComment = "Value input has changed"
                          , eventVariables = ["value"]
                          }
                        , Event
                          { eventName = "SETPOINT"
                          , eventComment = "Setpoint has changed"
                          , eventVariables = [ "value"
                                             , "setpoint"
                                             , "tOn"
                                             , "tOff"]
                          }
                        , Event
                          { eventName = "DELAYEXP"
                          , eventComment = "Delay Expired"
                          , eventVariables = []
                          }]
        , eventOutputs = [ Event
                           { eventName = "EOOB"
                           , eventComment = "OOB Event"
                           , eventVariables = ["OOB"]
                           }
                         , Event
                           { eventName = "START"
                           , eventComment = "Start delay event"
                           , eventVariables = ["tDelay"]
                           }
                         , Event
                           { eventName = "STOP"
                           , eventComment = "Stop delay event"
                           , eventVariables = []
                           }]
        , inputVariables = [ Variable
                             { variableName = "value"
                             , variableType = IECBool
                             , variableComment = "Input value"
                             }
                           , Variable
                             { variableName = "setpoint"
                             , variableType = IECBool
                             , variableComment = "Setpoint"
                             }
                           , Variable
                             { variableName = "tOn"
                             , variableType = IECInt Sixteen
                             , variableComment = "Granted time after setpoint becomes TRUE"
                             }
                           , Variable
                             { variableName = "tOff"
                             , variableType = IECInt Sixteen
                             , variableComment = "Granted time after setpoint becomes FALSE"
                             }]
        , outputVariables = [ Variable
                              { variableName = "OOB"
                              , variableType = IECBool
                              , variableComment = "Out of Bounds indication"
                              }
                            , Variable
                              { variableName = "tDelay"
                              , variableType = IECInt Sixteen
                              , variableComment = "Time value for delay"
                              }]
        }
      , bfbVariables = []
      , bfbStates = [ ECState
                      { ecStateName = "START"
                      , ecStateComment = "Initial State"
                      , ecStateActions = []
                      , ecStatePosition = 1912.941 :+ 97.4119
                      }
                    , ECState
                      { ecStateName = "OK"
                      , ecStateComment = ""
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = "ok"
                                           , ecActionOutput = "EOOB"
                                           }
                                         , ECAction
                                           { ecActionAlgorithm = ""
                                           , ecActionOutput = "STOP"
                                           }]
                      , ecStatePosition = 1276.0 :+ 740.0
                      }
                    , ECState
                      { ecStateName = "WAITING"
                      , ecStateComment = ""
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = "waiting"
                                           , ecActionOutput = "START"
                                           }]
                      , ecStatePosition = 2124.0 :+ 1480.0
                      }
                    , ECState
                      { ecStateName = "OOB"
                      , ecStateComment = ""
                      , ecStateActions = [ ECAction
                                           { ecActionAlgorithm = "oob"
                                           , ecActionOutput = "EOOB"
                                           }]
                      , ecStatePosition = 2780.0 :+ 768.0001
                      }]
      , bfbTransitions = [ ECTransition
                           { ecTransitionSource = "START"
                           , ecTransitionDestination = "OK"
                           , ecTransitionCondition = StBinaryOp
                                 StAnd
                                 (StLValue (SimpleLValue "SETPOINT"))
                                 (StSubValue
                                      (StBinaryOp
                                           StEquals
                                           (StLValue (SimpleLValue "setpoint"))
                                           (StLValue (SimpleLValue "value"))))
                           , ecTransitionPosition = 1528.318 :+ 340.9226
                           }
                         , ECTransition
                           { ecTransitionSource = "WAITING"
                           , ecTransitionDestination = "OOB"
                           , ecTransitionCondition = StLValue
                                 (SimpleLValue "DELAYEXP")
                           , ecTransitionPosition = 2638.756 :+ 1209.103
                           }
                         , ECTransition
                           { ecTransitionSource = "OK"
                           , ecTransitionDestination = "OOB"
                           , ecTransitionCondition = StBinaryOp
                                 StAnd
                                 (StLValue (SimpleLValue "VALUE"))
                                 (StSubValue
                                      (StBinaryOp
                                           StNotEquals
                                           (StLValue (SimpleLValue "value"))
                                           (StLValue (SimpleLValue "setpoint"))))
                           , ecTransitionPosition = 2053.028 :+ 454.6079
                           }
                         , ECTransition
                           { ecTransitionSource = "OOB"
                           , ecTransitionDestination = "OK"
                           , ecTransitionCondition = StBinaryOp
                                 StEquals
                                 (StLValue (SimpleLValue "value"))
                                 (StLValue (SimpleLValue "setpoint"))
                           , ecTransitionPosition = 2077.188 :+ 592.0204
                           }
                         , ECTransition
                           { ecTransitionSource = "START"
                           , ecTransitionDestination = "OOB"
                           , ecTransitionCondition = StBinaryOp
                                 StAnd
                                 (StLValue (SimpleLValue "SETPOINT"))
                                 (StSubValue
                                      (StBinaryOp
                                           StNotEquals
                                           (StLValue (SimpleLValue "setpoint"))
                                           (StLValue (SimpleLValue "value"))))
                           , ecTransitionPosition = 2626.277 :+ 261.6464
                           }
                         , ECTransition
                           { ecTransitionSource = "OK"
                           , ecTransitionDestination = "WAITING"
                           , ecTransitionCondition = StBinaryOp
                                 StAnd
                                 (StLValue (SimpleLValue "SETPOINT"))
                                 (StBinaryOp
                                      StNotEquals
                                      (StLValue (SimpleLValue "setpoint"))
                                      (StLValue (SimpleLValue "value")))
                           , ecTransitionPosition = 1071.692 :+ 1432.978
                           }
                         , ECTransition
                           { ecTransitionSource = "WAITING"
                           , ecTransitionDestination = "OK"
                           , ecTransitionCondition = StBinaryOp
                                 StEquals
                                 (StLValue (SimpleLValue "setpoint"))
                                 (StLValue (SimpleLValue "value"))
                           , ecTransitionPosition = 1590.343 :+ 1140.822
                           }
                         , ECTransition
                           { ecTransitionSource = "WAITING"
                           , ecTransitionDestination = "WAITING"
                           , ecTransitionCondition = StBinaryOp
                                 StAnd
                                 (StLValue (SimpleLValue "SETPOINT"))
                                 (StSubValue
                                      (StBinaryOp
                                           StNotEquals
                                           (StLValue (SimpleLValue "setpoint"))
                                           (StLValue (SimpleLValue "value"))))
                           , ecTransitionPosition = 2124.0 :+ 1658.336
                           }]
      , bfbAlgorithms = [ ECAlgorithm
                          { ecAlgorithmName = "ok"
                          , ecAlgorithmComment = ""
                          , ecAlgorithmStText = [ IfElse
                                                      (StLValue
                                                           (SimpleLValue "OOB"))
                                                      [ Assignment
                                                            (SimpleLValue "OOB")
                                                            (StBool False)]
                                                      [ Assignment
                                                            (SimpleLValue
                                                                 "EOOB")
                                                            (StBool False)]
                                                , IfElse
                                                      (StBinaryOp
                                                           StNotEquals
                                                           (StLValue
                                                                (SimpleLValue
                                                                     "tDelay"))
                                                           (StInt 0))
                                                      [ Assignment
                                                            (SimpleLValue
                                                                 "tDelay")
                                                            (StInt 0)]
                                                      [ Assignment
                                                            (SimpleLValue
                                                                 "STOP")
                                                            (StBool False)]]
                          }
                        , ECAlgorithm
                          { ecAlgorithmName = "waiting"
                          , ecAlgorithmComment = ""
                          , ecAlgorithmStText = [ Assignment
                                                      (SimpleLValue "tDelay")
                                                      (StFunc
                                                           "SEL"
                                                           [ StLValue
                                                                 (SimpleLValue
                                                                      "setpoint")
                                                           , StLValue
                                                                 (SimpleLValue
                                                                      "tOff")
                                                           , StLValue
                                                                 (SimpleLValue
                                                                      "tOn")])
                                                , Assignment
                                                      (SimpleLValue "SETPOINT")
                                                      (StBool False)]
                          }
                        , ECAlgorithm
                          { ecAlgorithmName = "oob"
                          , ecAlgorithmComment = ""
                          , ecAlgorithmStText = [ IfElse
                                                      (StMonoOp
                                                           StNot
                                                           (StLValue
                                                                (SimpleLValue
                                                                     "OOB")))
                                                      [ Assignment
                                                            (SimpleLValue "OOB")
                                                            (StBool True)]
                                                      [ Assignment
                                                            (SimpleLValue
                                                                 "EOOB")
                                                            (StBool False)]
                                                , If
                                                      (StBinaryOp
                                                           StNotEquals
                                                           (StLValue
                                                                (SimpleLValue
                                                                     "tDelay"))
                                                           (StInt 0))
                                                      [ Assignment
                                                            (SimpleLValue
                                                                 "tDelay")
                                                            (StInt 0)]]
                          }]
      }]
