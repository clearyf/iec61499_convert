module ButtonTimerComposite where

import BasePrelude
import ParseIec61499
import ParseSt

buttonTimerCompositeBlock :: [CompositeFunctionBlock]
buttonTimerCompositeBlock =
    [ CompositeFunctionBlock
      { cfbDescription = FunctionBlockDescription
        { fbName = "ButtonTimer"
        , fbComment = "Composite Function Block Type"
        , fbNamespace = "Loytec.Automation"
        }
      , cfbInterfaceList = InterfaceList
        { eventInputs = [ Event
                          { eventName = "BUTTON"
                          , eventComment = "Button event"
                          , eventVariables = ["button", "longSeconds"]
                          }]
        , eventOutputs = [ Event
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
                             , variableType = IECReal
                             , variableComment = "Time for long press"
                             }]
        , outputVariables = []
        }
      , cfbFunctionBlocks = [ FunctionBlockEntry
                              { fbeId = "1"
                              , fbeName = "logic"
                              , fbeType = "ButtonTimerLogic"
                              , fbeNamespace = "Loytec.Automation"
                              , fbeCoord = 980.0 :+ 80.0
                              }
                            , FunctionBlockEntry
                              { fbeId = "2"
                              , fbeName = "delay"
                              , fbeType = "E_DELAY"
                              , fbeNamespace = "IEC61499.Standard"
                              , fbeCoord = 1200.0 :+ 980.0
                              }]
      , cfbFunctionBlockIO = [ FunctionBlockIO
                               { fbioDirection = DirectionInput
                               , fbioName = "BUTTON"
                               , fbioCoord = 80.0 :+ 22.963543
                               , fbioType = FunctionBlockIOEvent
                               }
                             , FunctionBlockIO
                               { fbioDirection = DirectionInput
                               , fbioName = "button"
                               , fbioCoord = 80.0 :+ 147.96355
                               , fbioType = FunctionBlockIOData
                               }
                             , FunctionBlockIO
                               { fbioDirection = DirectionInput
                               , fbioName = "longSeconds"
                               , fbioCoord = 80.00001 :+ 162.96353
                               , fbioType = FunctionBlockIOData
                               }]
      , cfbEventConnections = [ EventConnection
                                { ecSource = "BUTTON"
                                , ecDestination = "logic.BUTTON"
                                , ecAttributes = []
                                }
                              , EventConnection
                                { ecSource = "logic.DELAY_START"
                                , ecDestination = "delay.START"
                                , ecAttributes = [CrossReference]
                                }
                              , EventConnection
                                { ecSource = "logic.DELAY_STOP"
                                , ecDestination = "delay.STOP"
                                , ecAttributes = [CrossReference]
                                }
                              , EventConnection
                                { ecSource = "delay.EO"
                                , ecDestination = "logic.DELAY_DONE"
                                , ecAttributes = [CrossReference]
                                }
                              , EventConnection
                                { ecSource = "logic.LONG"
                                , ecDestination = "LONG"
                                , ecAttributes = []
                                }
                              , EventConnection
                                { ecSource = "logic.SHORT"
                                , ecDestination = "SHORT"
                                , ecAttributes = []
                                }
                              , EventConnection
                                { ecSource = "logic.RELEASED"
                                , ecDestination = "RELEASED"
                                , ecAttributes = []
                                }
                              , EventConnection
                                { ecSource = "logic.PRESSED"
                                , ecDestination = "PRESSED"
                                , ecAttributes = []
                                }]
      , cfbDataConnections = [ DataConnection
                               { dcSource = "logic.delayTime"
                               , dcDestination = "delay.DT"
                               , dcAttributes = [CrossReference]
                               }
                             , DataConnection
                               { dcSource = "longSeconds"
                               , dcDestination = "logic.longSeconds"
                               , dcAttributes = []
                               }
                             , DataConnection
                               { dcSource = "button"
                               , dcDestination = "logic.button"
                               , dcAttributes = []
                               }]
      }]
