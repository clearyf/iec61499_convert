module ParseIec61499
       (readFunctionBlock, FunctionBlock(..), InterfaceList(..),
        BasicFunctionBlock(..), ECState(..), ECTransition(..),
        ECAction(..), ECAlgorithm(..), Event(..), Variable(..))
       where

import BasePrelude hiding (orElse)
import ParseSt (parseSt, Statement, IECVariable(..), iECtypeFromString)
import Text.XML.HXT.Core
       (ArrowXml, SysConfig, XmlTree, arr2, arr3, arr4, constA, deep,
        isElem, getAttrValue, hasName, listA, no, orElse, readDocument,
        root, runX, withOutputPLAIN, withValidate, writeDocumentToString,
        xread)

-- This represents the expected objects in the XML structure.
data FunctionBlock = FunctionBlock
    { fbName :: String
    , interfaceList :: InterfaceList
    , basicFb :: BasicFunctionBlock
    } deriving (Show,Eq)

data InterfaceList = InterfaceList
    { eventInputs :: [Event]
    , eventOutputs :: [Event]
    , inputVariables :: [Variable]
    , outputVariables :: [Variable]
    } deriving (Show,Eq)

data BasicFunctionBlock = BasicFunctionBlock
    { bfbVariables :: [Variable]
    , bfbStates :: [ECState]
    , bfbTransitions :: [ECTransition]
    , bfbAlgorithms :: [ECAlgorithm]
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
    , ecTransitionCondition :: String
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
    getAttrValue "Condition" &&&
    getCoords >>>
    arr4 ECTransition

-- Parses a string into an XmlTree, then writes it back out using the
-- 'withOutputPLAIN' option which disables all entity substitutions.
entityDecode :: String -> IO [String]
entityDecode str =
    runX
        (root [] [constA str >>> xread] >>>
         writeDocumentToString [withOutputPLAIN])

-- Uses entityDecode, but throws an error if there was a problem, and
-- uses unsafePerformIO to discard the IO monad.
unescapeXml :: String -> String
unescapeXml str =
    case unsafePerformIO (entityDecode str) of
        [a] -> a
        _ -> error ("Couldn't decode entities: " <> str)

getSt :: ArrowXml a => a String [Statement]
getSt =
    arr unescapeXml >>>
    arr parseSt >>^
    either (error . ("ST code in algorithm could not be parsed!" <>) . show) id

getAlgorithm :: ArrowXml a => a XmlTree ECAlgorithm
getAlgorithm =
    atTag "Algorithm" >>>
    getAttrValue "Name" &&&
    getAttrValueOrEmpty "Comment" &&&
    (atTag "ST" >>> getAttrValue "Text" >>> getSt) >>>
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
    atTag "BasicFB" >>>
    getListAtElem getVariable "InternalVars" &&&
    getListAtElem getECState "ECC" &&&
    getListAtElem getECTransition "ECC" &&&
    (listA getAlgorithm `orElse` constA mempty) >>>
    arr4 BasicFunctionBlock

getFunctionBlock :: ArrowXml a => a XmlTree FunctionBlock
getFunctionBlock =
    atTag "FBType" >>>
    getAttrValue "Name" &&& getInterfaceList &&& getBasicFunctionBlock >>>
    arr3 FunctionBlock

xmlOptions :: [SysConfig]
xmlOptions = [withValidate no]

readFunctionBlock :: FilePath -> IO [FunctionBlock]
readFunctionBlock path =
    runX (readDocument xmlOptions path >>> getFunctionBlock)
