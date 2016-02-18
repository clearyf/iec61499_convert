module UppaalTypes where

import BasePrelude

-- | Definition of one Uppaal class.  The Events & Vars become
-- template parameters, channels first, then the vars, all as
-- references.
data UppaalModel = UppaalModel
    { modelName :: String
    , modelInputEvents :: [UppaalChan]
    , modelOutputEvents :: [UppaalChan]
    , modelInputVars :: [UppaalVar]
    , modelOutputVars :: [UppaalVar]
    , modelLocations :: [Location]
    , modelTransitions :: [Transition]
    , modelInternalVars :: [UppaalVar]
    , modelDeclarations :: String
    } deriving (Show,Eq)

data UppaalVar = UppaalVar String String deriving (Show,Eq)

newtype UppaalChan = UppaalChan String deriving (Show,Eq)

newtype StateId  = StateId Int deriving (Show,Eq)

data Location
    = Location AState
    | UrgentLocation AState
    deriving (Show,Eq)

data Transition = Transition
    { transitionSrc :: StateId
    , transitionDest :: StateId
    , transitionSync :: Maybe String
    , transitionGuard :: Maybe String
    , transitionAssignment :: Maybe String
    } deriving (Show,Eq)

data AState = AState
    { stateName :: String
    , stateId :: StateId
    , statePosition :: Complex Float
    } deriving (Show,Eq)
