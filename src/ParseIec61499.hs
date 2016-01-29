module ParseIec61499
       (readBasicFunctionBlock, BasicFunctionBlock(..), InterfaceList(..),
        ECState(..), ECTransition(..), ECAction(..), ECAlgorithm(..),
        Event(..), Variable(..))
       where

import BasePrelude hiding (orElse)
import ParseSt
       (parseSt, parseValue, Statement, IECVariable(..),
        iECtypeFromString, Value(..))
import Text.XML.HXT.Core
       (ArrowXml, SysConfig, XmlTree, arr2, arr3, arr4, constA,
        deep, isElem, getAttrValue, hasName, listA, no, orElse,
        readDocument, runX, substAllXHTMLEntityRefs, withValidate)

-- This represents the expected objects in the XML structure.
data BasicFunctionBlock = BasicFunctionBlock
    { bfbName :: String
    , bfbInterfaceList :: InterfaceList
    , bfbVariables :: [Variable]
    , bfbStates :: [ECState]
    , bfbTransitions :: [ECTransition]
    , bfbAlgorithms :: [ECAlgorithm]
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
atTag tag = deep (isElem >>> hasName tag)

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

getCoords :: ArrowXml a => a XmlTree (Complex Float)
getCoords =
    getAttrValue "x" &&& getAttrValue "y" >>>
    arr2
        (\i j ->
              mkPolar (read i) (read j))

getListAtElem :: ArrowXml a => a XmlTree c -> String -> a XmlTree [c]
getListAtElem f tag = (listA f <<< deep (hasName tag)) `orElse` constA mempty

getECState :: ArrowXml a => a XmlTree ECState
getECState =
    atTag "ECState" >>>
    getAttrValue "Name" &&&
    getAttrValue "Comment" &&&
    listA getECAction `orElse` constA mempty &&&
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

-- We can presume that there is always an interface list element, but
-- some or all of the children of the interface list may be missing.
getInterfaceList :: ArrowXml a => a XmlTree InterfaceList
getInterfaceList =
    atTag "InterfaceList" >>>
    getListAtElem getEvent "EventInputs" &&&
    getListAtElem getEvent "EventOutputs" &&&
    getListAtElem getVariable "InputVars" &&&
    getListAtElem getVariable "OutputVars" >>>
    arr4 InterfaceList

-- We again presume the ECC is there, and there may or may not be a
-- number of algorithms.
getBasicFunctionBlock :: ArrowXml a => a XmlTree BasicFunctionBlock
getBasicFunctionBlock =
    atTag "FBType" >>>
    getAttrValue "Name" &&&
    getInterfaceList &&&
    (atTag "BasicFB" >>>
     getListAtElem getVariable "InternalVars" &&&
     getListAtElem getECState "ECC" &&&
     getListAtElem getECTransition "ECC" &&&
     (listA getAlgorithm `orElse` constA mempty)) >>>
    arr6 BasicFunctionBlock

-- Not implemented in HXT, but it’s a mechanical extension of arr4.
arr6 :: Arrow a => (t -> t1 -> t2 -> t3 -> t4 -> t5 -> c) -> a (t, (t1, (t2, (t3, (t4, t5))))) c
arr6 f = arr (\ ~(x1, ~(x2, ~(x3, ~(x4, ~(x5, ~(x6)))))) -> f x1 x2 x3 x4 x5 x6)

xmlOptions :: [SysConfig]
xmlOptions = [withValidate no]

readBasicFunctionBlock :: FilePath -> IO [BasicFunctionBlock]
readBasicFunctionBlock path =
    runX (readDocument xmlOptions path >>> getBasicFunctionBlock)
