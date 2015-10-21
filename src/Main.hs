{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import BasePrelude
import Control.Arrow.ArrowTree (deep)
import Data.Text (Text)
import Text.XML.HXT.Arrow.XmlArrow (ArrowXml)
import Text.XML.HXT.Core
       (isElem, hasName, listA, getChildren, getText, getName,
        readDocument, runX, withValidate, no, SysConfig)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)

-- This represents the expected objects in the XML structure.
data FunctionBlock = FunctionBlock
  { eventInputs :: [Event]
  , eventOutputs :: [Event]
  , inputVariables :: [Variable]
  , outputVariables :: [Variable]
  , basicFb :: BasicFunctionBlock
  }
  deriving (Show)

data BasicFunctionBlock = BasicFunctionBlock
  { ecStates :: [ECState]
  , ecAlgorithms :: [ECAlgorithm]
  }
  deriving (Show)

data ECState = ECState
  { ecStateName :: Text
  , ecStateComment :: Text
  , ecStateActions :: [ECActions]
  }
  deriving (Show)

data ECActions = ECActions
  { ecActionAlgorithm :: Text
  , ecActionOutput :: Text
  }
  deriving (Show)

data ECAlgorithm = ECAlgorithm
  { ecAlgorithmName :: Text
  , ecAlgorithmComment :: Text
  , ecAlgorithmStText :: Text
  }
  deriving (Show)

data Event = Event
  { eventName :: Text
  , eventVariables :: [Variable]
  }
  deriving (Show)

data Variable = Variable
  { variableName :: Text
  , variableType :: IECVariable
  , variableComment :: Text
  }
  deriving (Show)

data IECVariable
  = IECReal
  | IECInt
  deriving (Show)

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

parseInterfaceList :: ArrowXml a => a XmlTree [String]
parseInterfaceList = atTag "InterfaceList" >>> listA (getChildren >>> getName)

-- parseEvents :: ArrowXml a => String -> a XmlTree [String]
parseEvents event =
  atTag event >>>
  listA (getChildren >>> (getName &&& (getChildren >>> getText)))

xmlOptions :: [SysConfig]
xmlOptions = [withValidate no]

main :: IO ()
main = do let path = "examples/iec61449/toggle_fb.xml"
          result <- runX (readDocument xmlOptions path >>> parseEvents "InterfaceList")
          print result
