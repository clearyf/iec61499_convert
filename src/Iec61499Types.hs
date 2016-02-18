module Iec61499Types where

import BasePrelude
import StTypes (Statement, IECVariable(..), Value(..))

-- This represents the expected objects in the XML structure.
data BasicFunctionBlock = BasicFunctionBlock
    { bfbDescription :: FunctionBlockDescription
    , bfbInterfaceList :: InterfaceList
    , bfbVariables :: [Variable]
    , bfbStates :: [ECState]
    , bfbTransitions :: [ECTransition]
    , bfbAlgorithms :: [ECAlgorithm]
    } deriving (Show,Eq)

data CompositeFunctionBlock = CompositeFunctionBlock
    { cfbDescription :: FunctionBlockDescription
    , cfbInterfaceList :: InterfaceList
    , cfbFunctionBlocks :: [FunctionBlockEntry]
    , cfbFunctionBlockIO :: [FunctionBlockIO]
    , cfbEventConnections :: [EventConnection]
    , cfbDataConnections :: [DataConnection]
    } deriving (Show,Eq)

data FunctionBlockEntry = FunctionBlockEntry
    { fbeId :: String
    , fbeName :: String
    , fbeType :: String
    , fbeNamespace :: String
    , fbeCoord :: Complex Float
    } deriving (Show,Eq)

data FunctionBlockIODirection
    = DirectionInput
    | DirectionOutput
    deriving (Show,Eq)

data FunctionBlockIOType
    = FunctionBlockIOData
    | FunctionBlockIOEvent
    deriving (Show,Eq)

data FunctionBlockIO = FunctionBlockIO
    { fbioDirection :: FunctionBlockIODirection
    , fbioName :: String
    , fbioCoord :: Complex Float
    , fbioType :: FunctionBlockIOType
    } deriving (Show,Eq)

data ConnectionAttribute =
    CrossReference
    deriving (Show,Eq)

data EventConnection = EventConnection
    { ecSource :: String
    , ecDestination :: String
    , ecAttributes :: [ConnectionAttribute]
    } deriving (Show,Eq)

data DataConnection = DataConnection
    { dcSource :: String
    , dcDestination :: String
    , dcAttributes :: [ConnectionAttribute]
    } deriving (Show,Eq)

data FunctionBlockDescription = FunctionBlockDescription
    { fbName :: String
    , fbComment :: String
    , fbNamespace :: String
    } deriving (Show,Eq)

data InterfaceList = InterfaceList
    { eventInputs :: [Event]
    , eventOutputs :: [Event]
    , inputVariables :: [Variable]
    , outputVariables :: [Variable]
    } deriving (Show,Eq)

data ECState = ECState
    { ecStateName :: String
    , ecStateComment :: String
    , ecStateActions :: [ECAction]
    , ecStatePosition :: Complex Float
    } deriving (Show,Eq)

data ECTransition = ECTransition
    { ecTransitionSource :: String
    , ecTransitionDestination :: String
    , ecTransitionCondition :: Value
    , ecTransitionPosition :: Complex Float
    } deriving (Show,Eq)

data ECAction = ECAction
    { ecActionAlgorithm :: String
    , ecActionOutput :: String
    } deriving (Show,Eq)

data ECAlgorithm = ECAlgorithm
    { ecAlgorithmName :: String
    , ecAlgorithmComment :: String
    , ecAlgorithmStText :: [Statement]
    } deriving (Show,Eq)

data Event = Event
    { eventName :: String
    , eventComment :: String
    , eventVariables :: [String]
    } deriving (Show,Eq)

data Variable = Variable
    { variableName :: String
    , variableType :: IECVariable
    , variableComment :: String
    } deriving (Show,Eq)
