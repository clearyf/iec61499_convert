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
    , modelLocations = [ UrgentLocation
                             AState
                              { stateName = "__start_Off"
                              , stateId = StateId 4
                              }
                       , UrgentLocation
                             AState
                              { stateName = "__action_Off__Update_TurnOff_"
                              , stateId = StateId 5
                              }
                       , Location
                             AState
                              { stateName = "Off"
                              , stateId = StateId 6
                              }
                       , UrgentLocation
                             AState
                              { stateName = "__start_On"
                              , stateId = StateId 1
                              }
                       , UrgentLocation
                             AState
                              { stateName = "__action_On__Update_TurnOn_"
                              , stateId = StateId 2
                              }
                       , Location
                             AState
                              { stateName = "On"
                              , stateId = StateId 3
                              }
                       , Location
                             AState
                              { stateName = "START"
                              , stateId = StateId 0
                              }]
    , modelTransitions = [ Transition
                           { transitionSrc = StateId 0
                           , transitionDest = StateId 4
                           , transitionSync = mzero
                           , transitionUpdate = mzero
                           }
                         , Transition
                           { transitionSrc = StateId 6
                           , transitionDest = StateId 1
                           , transitionSync = pure "ic_Toggle?"
                           , transitionUpdate = mzero
                           }
                         , Transition
                           { transitionSrc = StateId 3
                           , transitionDest = StateId 4
                           , transitionSync = pure "ic_Toggle?"
                           , transitionUpdate = mzero
                           }
                         , Transition
                           { transitionSrc = StateId 1
                           , transitionDest = StateId 2
                           , transitionSync = pure "oc_Update!"
                           , transitionUpdate = pure "TurnOn();"
                           }
                         , Transition
                           { transitionSrc = StateId 2
                           , transitionDest = StateId 3
                           , transitionSync = mzero
                           , transitionUpdate = mzero
                           }
                         , Transition
                           { transitionSrc = StateId 4
                           , transitionDest = StateId 5
                           , transitionSync = pure "oc_Update!"
                           , transitionUpdate = pure "TurnOff();"
                           }
                         , Transition
                           { transitionSrc = StateId 5
                           , transitionDest = StateId 6
                           , transitionSync = mzero
                           , transitionUpdate = mzero
                           }]
    , modelDeclarations = [ "void TurnOn()\n{\n\tValue = true;\n}\n"
                          , "void TurnOff()\n{\n\tValue = false;\n}\n"]
    }
