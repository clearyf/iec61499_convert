{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import BasePrelude hiding (orElse)
import Text.XML.HXT.Core
       (ArrowXml, SysConfig, XmlTree, constA, deep, isElem, getAttrValue,
        hasName, listA, getChildren, getText, getName, no, orElse,
        readDocument, runX, withValidate)

-- This represents the expected objects in the XML structure.
data FunctionBlock = FunctionBlock
  { interfaceList :: InterfaceList
  , basicFb :: BasicFunctionBlock
  }
  deriving (Show)

data InterfaceList = InterfaceList
  { eventInputs :: [Event]
  , eventOutputs :: [Event]
  , inputVariables :: [Variable]
  , outputVariables :: [Variable]
  }
  deriving (Show)

data BasicFunctionBlock = BasicFunctionBlock
  { ecStates :: [ECC]
  , ecAlgorithms :: [ECAlgorithm]
  }
  deriving (Show)

data ECC = ECC
  { ecStateName :: String
  , ecStateComment :: String
  , ecStateActions :: [ECActions]
  }
  deriving (Show)

data ECActions = ECActions
  { ecActionAlgorithm :: String
  , ecActionOutput :: String
  }
  deriving (Show)

data ECAlgorithm = ECAlgorithm
  { ecAlgorithmName :: String
  , ecAlgorithmComment :: String
  , ecAlgorithmStText :: String
  }
  deriving (Show)

data Event = Event
  { eventName :: String
  , eventVariables :: [String]
  }
  deriving (Show)

data Variable = Variable
  { variableName :: String
  , variableType :: IECVariable
  , variableComment :: String
  }
  deriving (Show)

data IECVariable
  = IECReal
  | IECInt
  | IECBool
  deriving (Show)

vartypeFromString :: String -> IECVariable
vartypeFromString str
  | capsStr == "BOOL" = IECBool
  | capsStr == "REAL" = IECReal
  | capsStr == "INT" = IECInt
  | otherwise = error "Unhandled IEC variable type!"
  where capsStr = map toUpper str

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

parseEvents :: ArrowXml a => String -> a XmlTree [String]
parseEvents event = atTag event >>> listA (getChildren >>> getName)

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

getAlgorithm = atTag "Algorithm" >>>
  proc x -> do name <- getAttrValue "Name" -< x
               comment <- getAttrValue "Comment" -< x
               st <- atTag "ST" -< x
               stText <- getAttrValue "Text" -< st
               returnA -< ECAlgorithm name comment stText

getFunctionBlock :: ArrowXml a => a XmlTree FunctionBlock
getFunctionBlock =
  atTag "FBType" >>>
  proc x -> do ilist <- deep (hasName "InterfaceList") -< x
               -- We can presume that there is always an interface
               -- list element, but some or all of the children of the
               -- interface list may be missing.
               inputs <- getListAtElem getEvent "EventInputs" -< ilist
               outputs <- getListAtElem getEvent "EventOutputs" -< ilist
               inputVars <- getListAtElem getVariable "InputVars" -< ilist
               outputVars <- getListAtElem getVariable "OutputVars" -< ilist
--               fb <- getText <<< getChildren <<< deep (hasName "BasicFB") -< x
               algorithms <- getListAtElem getAlgorithm "BasicFB" -< x
               returnA -< FunctionBlock
                            (InterfaceList inputs outputs inputVars outputVars)
                            (BasicFunctionBlock [] algorithms)

xmlOptions :: [SysConfig]
xmlOptions = [withValidate no]

test :: IO [FunctionBlock]
test = do let path = "examples/iec61449/toggle_fb.xml"
          runX (readDocument xmlOptions path >>> getFunctionBlock)

main :: IO ()
main = do result <- test
          print result
