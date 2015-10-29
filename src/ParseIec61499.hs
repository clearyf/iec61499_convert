{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module ParseIec61499
       (readFunctionBlock, FunctionBlock(..), InterfaceList(..),
        BasicFunctionBlock(..), ECCElement(..), ECState(..),
        ECTransition(..), ECAction(..), ECAlgorithm(..), Event(..),
        Variable(..), IECVariable(..))
       where

import BasePrelude hiding (orElse)
import Text.XML.HXT.Core
       (ArrowXml, SysConfig, XmlTree, constA, deep, isElem, getAttrValue,
        hasName, listA, no, orElse, readDocument, runX, withValidate)

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
  , ecAlgorithmStText :: String
  }
  deriving (Show,Eq)

data Event = Event
  { eventName :: String
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

getEventVars :: ArrowXml a => a XmlTree String
getEventVars = atTag "With" >>> getAttrValue "Var"

getEvent :: ArrowXml a => a XmlTree Event
getEvent = atTag "Event" >>>
  proc x -> do name <- getAttrValue "Name" -< x
               vars <- listA getEventVars -< x
               returnA -< Event name vars

getVariable :: ArrowXml a => a XmlTree Variable
getVariable = atTag "VarDeclaration" >>>
  proc x -> do name <- getAttrValue "Name" -< x
               vartype <- getAttrValue "Type" -< x
               comment <- getAttrValue "Comment" -< x
               returnA -< Variable name (vartypeFromString vartype) comment

getListAtElem :: ArrowXml a => a XmlTree c -> String -> a XmlTree [c]
getListAtElem f tag = (listA f <<< deep (hasName tag)) `orElse` constA []

getECCElement :: ArrowXml a => a XmlTree ECCElement
getECCElement = getECState <+> getECTransition

getECState :: ArrowXml a => a XmlTree ECCElement
getECState = atTag "ECState" >>>
  proc x -> do name <- getAttrValue "Name" -< x
               comment <- getAttrValue "Comment" -< x
               actions <- (listA getECAction) `orElse` constA [] -< x
               returnA -< ECCState (ECState name comment actions)

getECAction :: ArrowXml a => a XmlTree ECAction
getECAction = atTag "ECAction" >>>
  proc x -> do algorithm <- getAttrValue "Algorithm" -< x
               output <- getAttrValue "Output" -< x
               returnA -< ECAction algorithm output

getECTransition :: ArrowXml a => a XmlTree ECCElement
getECTransition = atTag "ECTransition" >>>
  proc x -> do source <- getAttrValue "Source" -< x
               destination <- getAttrValue "Destination" -< x
               condition <- getAttrValue "Condition" -< x
               returnA -<
                 ECCTransition (ECTransition source destination condition)

getAlgorithm :: ArrowXml a => a XmlTree ECAlgorithm
getAlgorithm = atTag "Algorithm" >>>
  proc x -> do name <- getAttrValue "Name" -< x
               comment <- getAttrValue "Comment" -< x
               st <- atTag "ST" -< x
               stText <- getAttrValue "Text" -< st
               returnA -< ECAlgorithm name comment stText

getFunctionBlock :: ArrowXml a => a XmlTree FunctionBlock
getFunctionBlock =
  atTag "FBType" >>>
  proc x -> do ilist <- atTag "InterfaceList" -< x
               -- We can presume that there is always an interface
               -- list element, but some or all of the children of the
               -- interface list may be missing.
               inputs <- getListAtElem getEvent "EventInputs" -< ilist
               outputs <- getListAtElem getEvent "EventOutputs" -< ilist
               inputVars <- getListAtElem getVariable "InputVars" -< ilist
               outputVars <- getListAtElem getVariable "OutputVars" -< ilist
               -- We again presume the ECC is there, and there may or
               -- may not be a number of algorithms.
               fb <- atTag "BasicFB" -< x
               elements <- getListAtElem getECCElement "ECC" -< fb
               algorithms <- getListAtElem getAlgorithm "BasicFB" -< x
               returnA -< FunctionBlock
                            (InterfaceList inputs outputs inputVars outputVars)
                            (BasicFunctionBlock elements algorithms)

xmlOptions :: [SysConfig]
xmlOptions = [withValidate no]

readFunctionBlock :: FilePath -> IO [FunctionBlock]
readFunctionBlock path =
  runX (readDocument xmlOptions path >>> getFunctionBlock)
