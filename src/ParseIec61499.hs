module ParseIec61499
       (readBasicFunctionBlock, readCompositeFunctionBlock,
        BasicFunctionBlock(..), CompositeFunctionBlock(..),
        FunctionBlockEntry(..), FunctionBlockIODirection(..),
        FunctionBlockIO(..), FunctionBlockIOType(..),
        ConnectionAttribute(..), EventConnection(..), DataConnection(..),
        FunctionBlockDescription(..), InterfaceList(..), ECState(..),
        ECTransition(..), ECAction(..), ECAlgorithm(..), Event(..),
        Variable(..))
       where

import BasePrelude hiding (orElse)
import ParseSt
       (parseSt, parseValue, Statement, IECVariable(..),
        iECtypeFromString, Value(..))
import Text.XML.HXT.Core
       (ArrowList, ArrowXml, IOSLA, SysConfig, XIOState, XmlTree, arr2,
        arr3, arr4, constA, isElem, isText, getAttrValue, getChildren,
        getText, hasAttrValue, hasName, listA, no, orElse, readDocument,
        runX, substAllXHTMLEntityRefs, withSubstDTDEntities, withValidate)

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

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = getChildren >>> isElem >>> hasName tag

getAttrValueOrEmpty :: ArrowXml a => String -> a XmlTree String
getAttrValueOrEmpty str = getAttrValue str `orElse` constA ""

getEventVars :: ArrowXml a => a XmlTree String
getEventVars = atTag "With" >>> getAttrValue "Var"

getEvent :: ArrowXml a => a XmlTree Event
getEvent =
    atTag "Event" >>>
    getAttrValue "Name" &&&
    getAttrValueOrEmpty "Comment" &&& listA getEventVars >>>
    arr3 Event

getVariable :: ArrowXml a => a XmlTree Variable
getVariable =
    atTag "VarDeclaration" >>>
    getAttrValue "Name" &&&
    (getAttrValue "Type" >>^ either (error . show) id . iECtypeFromString) &&&
    getAttrValueOrEmpty "Comment" >>>
    arr3 Variable

createFloat :: ArrowList a => a (String, String) (Complex Float)
createFloat = arr2 f
  where
    f i j = read i :+ read j

getCoords :: ArrowXml a => a XmlTree (Complex Float)
getCoords =
    getAttrValue "x" &&& getAttrValue "y" >>> createFloat

getList :: ArrowXml a => a XmlTree c -> a XmlTree [c]
getList f = listA f `orElse` constA mempty

-- Implementing getListAtElem in terms of getList is not completely
-- correct, as if the atTag fails then the orElse branch is still
-- required, as getList only helps if atTag succeeded.
getListAtElem :: ArrowXml a => a XmlTree c -> String -> a XmlTree [c]
getListAtElem f tag = (atTag tag >>> listA f) `orElse` constA mempty

getECState :: ArrowXml a => a XmlTree ECState
getECState =
    atTag "ECState" >>>
    getAttrValue "Name" &&&
    getAttrValue "Comment" &&&
    getList getECAction &&&
    getCoords >>>
    arr4 ECState

getECAction :: ArrowXml a => a XmlTree ECAction
getECAction =
    atTag "ECAction" >>>
    getAttrValue "Algorithm" &&& getAttrValue "Output" >>> arr2 ECAction

getECTransition :: ArrowXml a => a XmlTree ECTransition
getECTransition =
    atTag "ECTransition" >>>
    getAttrValue "Source" &&&
    getAttrValue "Destination" &&&
    (getAttrValue "Condition" >>^ either (error . show) id . parseValue) &&&
    getCoords >>>
    arr4 ECTransition

getSt :: String -> [Statement]
getSt str =
    parseSt str &
    either
        (error . ("ST code in algorithm could not be parsed: " <>) . show)
        id

getAlgorithm :: ArrowXml a => a XmlTree ECAlgorithm
getAlgorithm =
    atTag "Algorithm" >>>
    getAttrValue "Name" &&&
    getAttrValueOrEmpty "Comment" &&&
    (atTag "ST" >>> substAllXHTMLEntityRefs >>> getAttrValue "Text" >>^ getSt) >>>
    arr3 ECAlgorithm

getInterfaceList :: ArrowXml a => a XmlTree InterfaceList
getInterfaceList =
    atTag "InterfaceList" >>>
    getListAtElem getEvent "EventInputs" &&&
    getListAtElem getEvent "EventOutputs" &&&
    getListAtElem getVariable "InputVars" &&&
    getListAtElem getVariable "OutputVars" >>>
    arr4 InterfaceList

getFunctionBlockDescription :: ArrowXml a => a XmlTree FunctionBlockDescription
getFunctionBlockDescription =
    getAttrValue "Name" &&&
    getAttrValueOrEmpty "Comment" &&&
    getAttrValue "Namespace" >>>
    arr3 FunctionBlockDescription

getBasicFunctionBlock :: ArrowXml a => a XmlTree BasicFunctionBlock
getBasicFunctionBlock =
    atTag "FBType" >>>
    getFunctionBlockDescription &&&
    getInterfaceList &&&
    (atTag "BasicFB" >>>
     getListAtElem getVariable "InternalVars" &&&
     getListAtElem getECState "ECC" &&&
     getListAtElem getECTransition "ECC" &&&
     getList getAlgorithm) >>>
    arr6 BasicFunctionBlock

-- Not implemented in HXT, but it’s a mechanical extension of arr4.
arr5 :: Arrow a => (t -> t1 -> t2 -> t3 -> t4 -> c) -> a (t, (t1, (t2, (t3, t4)))) c
arr5 f = arr (\ ~(x1, ~(x2, ~(x3, ~(x4, x5)))) -> f x1 x2 x3 x4 x5)

arr6 :: Arrow a => (t -> t1 -> t2 -> t3 -> t4 -> t5 -> c) -> a (t, (t1, (t2, (t3, (t4, t5))))) c
arr6 f = arr (\ ~(x1, ~(x2, ~(x3, ~(x4, ~(x5, x6))))) -> f x1 x2 x3 x4 x5 x6)

getFunctionBlock :: ArrowXml a => a XmlTree FunctionBlockEntry
getFunctionBlock =
    atTag "FB" >>>
    getAttrValue "ID" &&&
    getAttrValue "Name" &&&
    getAttrValue "Type" &&& getAttrValue "Namespace" &&& getCoords >>>
    arr5 FunctionBlockEntry

getFunctionBlockIO :: ArrowXml a => a XmlTree FunctionBlockIO
getFunctionBlockIO =
    getBlock "Input" DirectionInput `orElse` getBlock "Output" DirectionOutput
  where
    getBlock str dir =
        atTag str >>>
        getAttrValue "Name" &&&
        (atTag "Position" >>> getCoord) &&& (getTextAt "IsType" >>^ getType) >>>
        arr3 (FunctionBlockIO dir)
    getType str =
        case str of
            "Event" -> FunctionBlockIOEvent
            "Data" -> FunctionBlockIOData
            _ -> error ("Unknown FunctionBlock direction" <> str)
    getTextAt tag = atTag tag >>> getChildren >>> isText >>> getText
    getCoord = getTextAt "X" &&& getTextAt "Y" >>> createFloat

getCompositeFunctionBlock :: ArrowXml a => a XmlTree CompositeFunctionBlock
getCompositeFunctionBlock =
    atTag "FBType" >>>
    getFunctionBlockDescription &&&
    getInterfaceList &&&
    (atTag "FBNetwork" >>>
     getList getFunctionBlock &&&
     getList getFunctionBlockIO &&&
     getListAtElem (getConnection EventConnection) "EventConnections" &&&
     getListAtElem (getConnection DataConnection) "DataConnections") >>>
    arr6 CompositeFunctionBlock

getConnection :: ArrowXml a => (String -> String -> [ConnectionAttribute] -> c) -> a XmlTree c
getConnection f =
    atTag "Connection" >>>
    getAttrValue "Source" &&&
    getAttrValue "Destination" &&& (getAttribute `orElse` constA []) >>>
    arr3 f
  where
    getAttribute =
        atTag "Attribute" >>>
        hasAttrValue "Name" (== "Configuration.Connections.CrossReference") >>>
        hasAttrValue "Value" (== "True") >>> constA [CrossReference]

xmlOptions :: [SysConfig]
xmlOptions = [withValidate no, withSubstDTDEntities no]

readBlock :: IOSLA (XIOState ()) XmlTree c -> String -> IO [c]
readBlock function path = runX (readDocument xmlOptions path >>> function)

readBasicFunctionBlock :: FilePath -> IO [BasicFunctionBlock]
readBasicFunctionBlock = readBlock getBasicFunctionBlock

readCompositeFunctionBlock :: FilePath -> IO [CompositeFunctionBlock]
readCompositeFunctionBlock = readBlock getCompositeFunctionBlock
