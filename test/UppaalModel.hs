module UppaalModel where

import BasePrelude
import OutputUppaal

uppaalModel :: UppaalModel
uppaalModel =
    UppaalModel
    { modelName = "Toggle"
    , modelInputEvents = [UppaalChan "ic_Toggle"]
    , modelOutputEvents = [UppaalChan "oc_Update"]
    , modelInputVars = []
    , modelOutputVars = [UppaalVar "bool" "Value"]
    , modelInternalVars = [UppaalVar "int[0,65536]" "Blah"]
    , modelLocations = [ UrgentLocation
                             AState
                              { stateName = "__start_Off"
                              , stateId = StateId 4
                              , statePosition = 646.1995 :+ 894.9202
                              }
                       , UrgentLocation
                             AState
                              { stateName = "__action_Off__Update_TurnOff_"
                              , stateId = StateId 5
                              , statePosition = 676.1995 :+ 894.9202
                              }
                       , Location
                             AState
                              { stateName = "Off"
                              , stateId = StateId 6
                              , statePosition = 706.1995 :+ 894.9202
                              }
                       , UrgentLocation
                             AState
                              { stateName = "__start_On"
                              , stateId = StateId 1
                              , statePosition = 49.98699 :+ (-187.8113)
                              }
                       , UrgentLocation
                             AState
                              { stateName = "__action_On__Update_TurnOn_"
                              , stateId = StateId 2
                              , statePosition = 79.98699 :+ (-187.8113)
                              }
                       , Location
                             AState
                              { stateName = "On"
                              , stateId = StateId 3
                              , statePosition = 109.98699 :+ (-187.8113)
                              }
                       , Location
                             AState
                              { stateName = "START"
                              , stateId = StateId 0
                              , statePosition = (-305.01816) :+ 461.2027
                              }]
    , modelTransitions = [ Transition
                           { transitionSrc = StateId 0
                           , transitionDest = StateId 4
                           , transitionSync = mzero
                           , transitionGuard = mzero
                           , transitionUpdate = mzero
                           }
                         , Transition
                           { transitionSrc = StateId 6
                           , transitionDest = StateId 1
                           , transitionSync = pure "ic_Toggle?"
                           , transitionGuard = mzero
                           , transitionUpdate = mzero
                           }
                         , Transition
                           { transitionSrc = StateId 3
                           , transitionDest = StateId 4
                           , transitionSync = pure "ic_Toggle?"
                           , transitionGuard = mzero
                           , transitionUpdate = mzero
                           }
                         , Transition
                           { transitionSrc = StateId 1
                           , transitionDest = StateId 2
                           , transitionSync = pure "oc_Update!"
                           , transitionGuard = mzero
                           , transitionUpdate = pure "TurnOn();"
                           }
                         , Transition
                           { transitionSrc = StateId 2
                           , transitionDest = StateId 3
                           , transitionSync = mzero
                           , transitionGuard = mzero
                           , transitionUpdate = mzero
                           }
                         , Transition
                           { transitionSrc = StateId 4
                           , transitionDest = StateId 5
                           , transitionSync = pure "oc_Update!"
                           , transitionGuard = mzero
                           , transitionUpdate = pure "TurnOff();"
                           }
                         , Transition
                           { transitionSrc = StateId 5
                           , transitionDest = StateId 6
                           , transitionSync = mzero
                           , transitionGuard = mzero
                           , transitionUpdate = mzero
                           }]
    , modelDeclarations = [ "void TurnOn()\n{\n\tValue = true;\n}\n\n"
                          , "void TurnOff()\n{\n\tValue = false;\n}\n\n"]
    }
