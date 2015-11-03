{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module ParseIec61499
       (readFunctionBlock, FunctionBlock(..), InterfaceList(..),
        BasicFunctionBlock(..), ECCElement(..), ECState(..),
        ECTransition(..), ECAction(..), ECAlgorithm(..), Event(..),
        Variable(..), IECVariable(..))
       where

import BasePrelude hiding (orElse)
import Text.XML.HXT.Core
       (ArrowXml, SysConfig, XmlTree, arr2, arr3, arr4, constA, deep,
        isElem, getAttrValue, hasName, listA, no, orElse, readDocument,
        runX, withValidate)
import ParseSt (parseSt, Statement)

-- This represents the expected objects in the XML structure.
data FunctionBlock = FunctionBlock
  { interfaceList :: InterfaceList
  , basicFb :: BasicFunctionBlock
  }
  deriving (Show,Eq)

data InterfaceList = InterfaceList
  { eventInputs :: [Event]
  , eventOutputs :: [Event]
  , inputVariables :: [Variable]
  , outputVariables :: [Variable]
  }
  deriving (Show,Eq)

data BasicFunctionBlock = BasicFunctionBlock
  { bfbElements :: [ECCElement]
  , bfbAlgorithms :: [ECAlgorithm]
  }
  deriving (Show,Eq)

data ECCElement
  = ECCState ECState
  | ECCTransition ECTransition
  deriving (Show,Eq)

data ECState = ECState
  { ecStateName :: String
  , ecStateComment :: String
  , ecStateActions :: [ECAction]
  }
  deriving (Show,Eq)

data ECTransition = ECTransition
  { ecTransitionSource :: String
  , ecTransitionDestination :: String
  , ecTransitionCondition :: String
  }
  deriving (Show,Eq)

data ECAction = ECAction
  { ecActionAlgorithm :: String
  , ecActionOutput :: String
  }
  deriving (Show,Eq)

data ECAlgorithm = ECAlgorithm
  { ecAlgorithmName :: String
  , ecAlgorithmComment :: String
  , ecAlgorithmStText :: [Statement]
  }
  deriving (Show,Eq)

data Event = Event
  { eventName :: String
  , eventComment :: String
  , eventVariables :: [String]
  }
  deriving (Show,Eq)

data Variable = Variable
  { variableName :: String
  , variableType :: IECVariable
  , variableComment :: String
  }
  deriving (Show,Eq)

data IECVariable
  = IECReal
  | IECInt
  | IECBool
  deriving (Show,Eq)

vartypeFromString :: String -> IECVariable
vartypeFromString str
  | capsStr == "BOOL" = IECBool
  | capsStr == "REAL" = IECReal
  | capsStr == "INT" = IECInt
  | otherwise = error "Unhandled IEC variable type!"
  where capsStr = map toUpper str

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

getAttrValueOrEmpty :: ArrowXml a => String -> a XmlTree String
getAttrValueOrEmpty str = (getAttrValue str) `orElse` (constA "")

getEventVars :: ArrowXml a => a XmlTree String
getEventVars = atTag "With" >>> getAttrValue "Var"

getEvent :: ArrowXml a => a XmlTree Event
getEvent =
  atTag "Event" >>>
  getAttrValue "Name" &&& getAttrValueOrEmpty "Comment" &&& listA getEventVars >>>
  arr3 Event

getVariable :: ArrowXml a => a XmlTree Variable
getVariable =
  atTag "VarDeclaration" >>>
  getAttrValue "Name" &&&
  (getAttrValue "Type" >>> arr vartypeFromString) &&&
  getAttrValueOrEmpty "Comment" >>>
  arr3 Variable

getListAtElem :: ArrowXml a => a XmlTree c -> String -> a XmlTree [c]
getListAtElem f tag = (listA f <<< deep (hasName tag)) `orElse` constA []

getECCElement :: ArrowXml a => a XmlTree ECCElement
getECCElement = getECState <+> getECTransition

getECState :: ArrowXml a => a XmlTree ECCElement
getECState =
  atTag "ECState" >>>
  getAttrValue "Name" &&&
  getAttrValue "Comment" &&& (listA getECAction) `orElse` constA [] >>>
  arr3 ECState >>> arr ECCState

getECAction :: ArrowXml a => a XmlTree ECAction
getECAction =
  atTag "ECAction" >>>
  getAttrValue "Algorithm" &&& getAttrValue "Output" >>> arr2 ECAction

getECTransition :: ArrowXml a => a XmlTree ECCElement
getECTransition =
  atTag "ECTransition" >>>
  getAttrValue "Source" &&&
  getAttrValue "Destination" &&& getAttrValue "Condition" >>>
  arr3 ECTransition >>> arr ECCTransition

getSt :: ArrowXml a => a String [Statement]
getSt =
  arr parseSt >>^
  either (const (error "ST code in algorithm could not be parsed!")) id

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
  getListAtElem getECCElement "ECC" &&& (listA getAlgorithm `orElse` constA []) >>>
  arr2 BasicFunctionBlock

getFunctionBlock :: ArrowXml a => a XmlTree FunctionBlock
getFunctionBlock =
  atTag "FBType" >>>
  getInterfaceList &&& getBasicFunctionBlock >>> arr2 FunctionBlock

xmlOptions :: [SysConfig]
xmlOptions = [withValidate no]

readFunctionBlock :: FilePath -> IO [FunctionBlock]
readFunctionBlock path =
  runX (readDocument xmlOptions path >>> getFunctionBlock)
