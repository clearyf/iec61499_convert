module Demux8 where

import BasePrelude
import Iec61499Types
import StTypes

demux8Block :: [BasicFunctionBlock]
demux8Block =
    [ BasicFunctionBlock
      { bfbDescription = FunctionBlockDescription "Demux8" "Basic Function Block Type" "Loytec.HVAC"
      , bfbInterfaceList = InterfaceList
        { eventInputs = [ Event
                          { eventName = "REQ"
                          , eventComment = "Normal Execution Request"
                          , eventVariables = ["inValue"]
                          }]
        , eventOutputs = [ Event
                           { eventName = "CNF"
                           , eventComment = "Execution Confirmation"
                           , eventVariables = [ "outValue1"
                                              , "outValue2"
                                              , "outValue3"
                                              , "outValue4"
                                              , "outValue5"
                                              , "outValue6"
                                              , "outValue7"
                                              , "outValue8"]
                           }]
        , inputVariables = [ Variable
                             { variableName = "inValue"
                             , variableType = IECInt Sixteen
                             , variableComment = "Input Value (from 1 to 8)"
                             }]
        , outputVariables = [ Variable
                              { variableName = "outValue1"
                              , variableType = IECBool
                              , variableComment = "Output TRUE if inValue = 1"
                              }
                            , Variable
                              { variableName = "outValue2"
                              , variableType = IECBool
                              , variableComment = "Output TRUE if inValue = 2"
                              }
                            , Variable
                              { variableName = "outValue3"
                              , variableType = IECBool
                              , variableComment = "Output TRUE if inValue = 3"
                              }
                            , Variable
                              { variableName = "outValue4"
                              , variableType = IECBool
                              , variableComment = "Output TRUE if inValue = 4"
                              }
                            , Variable
                              { variableName = "outValue5"
                              , variableType = IECBool
                              , variableComment = "Output TRUE if inValue = 5"
                              }
                            , Variable
                              { variableName = "outValue6"
                              , variableType = IECBool
                              , variableComment = "Output TRUE if inValue = 6"
                              }
                            , Variable
                              { variableName = "outValue7"
                              , variableType = IECBool
                              , variableComment = "Output TRUE if inValue = 7"
                              }
                            , Variable
                              { variableName = "outValue8"
                              , variableType = IECBool
                              , variableComment = "Output TRUE if inValue = 8"
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
                          , ecAlgorithmStText = theAlgorithm
                          }]
      }]

theAlgorithm :: [Statement]
theAlgorithm =
    [ IfElse
          (StBinaryOp StEquals (StLValue (SimpleLValue "inValue")) (StInt 1))
          [Assignment (SimpleLValue "outValue1") (StBool True)]
          [Assignment (SimpleLValue "outValue1") (StBool False)]
    , IfElse
          (StBinaryOp StEquals (StLValue (SimpleLValue "inValue")) (StInt 2))
          [Assignment (SimpleLValue "outValue2") (StBool True)]
          [Assignment (SimpleLValue "outValue2") (StBool False)]
    , IfElse
          (StBinaryOp StEquals (StLValue (SimpleLValue "inValue")) (StInt 3))
          [Assignment (SimpleLValue "outValue3") (StBool True)]
          [Assignment (SimpleLValue "outValue3") (StBool False)]
    , IfElse
          (StBinaryOp StEquals (StLValue (SimpleLValue "inValue")) (StInt 4))
          [Assignment (SimpleLValue "outValue4") (StBool True)]
          [Assignment (SimpleLValue "outValue4") (StBool False)]
    , IfElse
          (StBinaryOp StEquals (StLValue (SimpleLValue "inValue")) (StInt 5))
          [Assignment (SimpleLValue "outValue5") (StBool True)]
          [Assignment (SimpleLValue "outValue5") (StBool False)]
    , IfElse
          (StBinaryOp StEquals (StLValue (SimpleLValue "inValue")) (StInt 6))
          [Assignment (SimpleLValue "outValue6") (StBool True)]
          [Assignment (SimpleLValue "outValue6") (StBool False)]
    , IfElse
          (StBinaryOp StEquals (StLValue (SimpleLValue "inValue")) (StInt 7))
          [Assignment (SimpleLValue "outValue7") (StBool True)]
          [Assignment (SimpleLValue "outValue7") (StBool False)]
    , IfElse
          (StBinaryOp StEquals (StLValue (SimpleLValue "inValue")) (StInt 8))
          [Assignment (SimpleLValue "outValue8") (StBool True)]
          [Assignment (SimpleLValue "outValue8") (StBool False)]]
