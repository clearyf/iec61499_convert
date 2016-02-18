module ParseIec61499
       (readBasicFunctionBlock, readCompositeFunctionBlock) where

import BasePrelude hiding (orElse)
import Iec61499Types
       (BasicFunctionBlock(..), CompositeFunctionBlock(..),
        FunctionBlockEntry(..), FunctionBlockIODirection(..),
        FunctionBlockIO(..), FunctionBlockIOType(..),
        ConnectionAttribute(..), EventConnection(..), DataConnection(..),
        FunctionBlockDescription(..), InterfaceList(..), ECState(..),
        ECTransition(..), ECAction(..), ECAlgorithm(..), Event(..),
        Variable(..))
import ParseSt (parseSt, parseValue, iECtypeFromString)
import Text.XML.HXT.Core
       (ArrowList, ArrowXml, IOSLA, SysConfig, XIOState, XmlTree, arr2,
        arr3, arr4, constA, isElem, isText, getAttrValue, getChildren,
        getText, hasAttrValue, hasName, listA, no, orElse, readDocument,
        runX, substAllXHTMLEntityRefs, withSubstDTDEntities, withValidate)

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

getVariable :: ArrowXml a => a XmlTree (Either String Variable)
getVariable =
    atTag "VarDeclaration" >>>
    getAttrValue "Name" &&&
    (getAttrValue "Type" >>^ iECtypeFromString) &&&
    getAttrValueOrEmpty "Comment" >>>
    arr3 createVariable
  where
    createVariable a b c = Variable <$> pure a <*> b <*> pure c

createFloat :: ArrowList a => a (String, String) (Either String (Complex Float))
createFloat = arr2 f
  where
    f i j = (:+) <$> readEither i <*> readEither j

getCoords :: ArrowXml a => a XmlTree (Either String (Complex Float))
getCoords =
    getAttrValue "x" &&& getAttrValue "y" >>> createFloat

getList :: ArrowXml a => a XmlTree c -> a XmlTree [c]
getList f = listA f `orElse` constA mempty

-- Implementing getListAtElem in terms of getList is not completely
-- correct, as if the atTag fails then the orElse branch is still
-- required, as getList only helps if atTag succeeded.
getListAtElem :: ArrowXml a => a XmlTree c -> String -> a XmlTree [c]
getListAtElem f tag = (atTag tag >>> listA f) `orElse` constA mempty

getECState :: ArrowXml a => a XmlTree (Either String ECState)
getECState =
    atTag "ECState" >>>
    getAttrValue "Name" &&&
    getAttrValue "Comment" &&&
    getList getECAction &&&
    getCoords >>>
    arr4 createState
  where
    createState a b c d = ECState <$> pure a <*> pure b <*> pure c <*> d

getECAction :: ArrowXml a => a XmlTree ECAction
getECAction =
    atTag "ECAction" >>>
    getAttrValue "Algorithm" &&& getAttrValue "Output" >>> arr2 ECAction

getECTransition :: ArrowXml a => a XmlTree (Either String ECTransition)
getECTransition =
    atTag "ECTransition" >>>
    getAttrValue "Source" &&&
    getAttrValue "Destination" &&&
    (getAttrValue "Condition" >>^ parseValue) &&& getCoords >>>
    arr4 createTransition
  where
    createTransition a b c d =
        ECTransition <$> pure a <*> pure b <*> c <*> d

getAlgorithm :: ArrowXml a => a XmlTree (Either String ECAlgorithm)
getAlgorithm =
    atTag "Algorithm" >>>
    getAttrValue "Name" &&&
    getAttrValueOrEmpty "Comment" &&&
    (atTag "ST" >>>
     substAllXHTMLEntityRefs >>> getAttrValue "Text" >>^ parseSt "ST Algorithm") >>>
    arr3 createAlgorithm
  where
    createAlgorithm a b c = ECAlgorithm <$> pure a <*> pure b <*> c

getInterfaceList :: ArrowXml a => a XmlTree (Either String InterfaceList)
getInterfaceList =
    atTag "InterfaceList" >>>
    getListAtElem getEvent "EventInputs" &&&
    getListAtElem getEvent "EventOutputs" &&&
    getListAtElem getVariable "InputVars" &&&
    getListAtElem getVariable "OutputVars" >>>
    arr4 createAddressList
  where
    createAddressList a b c d =
        InterfaceList <$> pure a <*> pure b <*> sequence c <*> sequence d

getFunctionBlockDescription :: ArrowXml a => a XmlTree FunctionBlockDescription
getFunctionBlockDescription =
    getAttrValue "Name" &&&
    getAttrValueOrEmpty "Comment" &&&
    getAttrValue "Namespace" >>>
    arr3 FunctionBlockDescription

getBasicFunctionBlock :: ArrowXml a => a XmlTree (Either String BasicFunctionBlock)
getBasicFunctionBlock =
    atTag "FBType" >>>
    getFunctionBlockDescription &&&
    getInterfaceList &&&
    (atTag "BasicFB" >>>
     getListAtElem getVariable "InternalVars" &&&
     getListAtElem getECState "ECC" &&&
     getListAtElem getECTransition "ECC" &&& getList getAlgorithm) >>>
    arr6 createBasicFunctionBlock
  where
    createBasicFunctionBlock a b c d e f =
        BasicFunctionBlock <$> pure a
                           <*> b
                           <*> sequence c
                           <*> sequence d
                           <*> sequence e
                           <*> sequence f

-- Not implemented in HXT, but it’s a mechanical extension of arr4.
arr5 :: Arrow a => (t -> t1 -> t2 -> t3 -> t4 -> c) -> a (t, (t1, (t2, (t3, t4)))) c
arr5 f = arr (\ ~(x1, ~(x2, ~(x3, ~(x4, x5)))) -> f x1 x2 x3 x4 x5)

arr6 :: Arrow a => (t -> t1 -> t2 -> t3 -> t4 -> t5 -> c) -> a (t, (t1, (t2, (t3, (t4, t5))))) c
arr6 f = arr (\ ~(x1, ~(x2, ~(x3, ~(x4, ~(x5, x6))))) -> f x1 x2 x3 x4 x5 x6)

getFunctionBlock :: ArrowXml a => a XmlTree (Either String FunctionBlockEntry)
getFunctionBlock =
    atTag "FB" >>>
    getAttrValue "ID" &&&
    getAttrValue "Name" &&&
    getAttrValue "Type" &&& getAttrValue "Namespace" &&& getCoords >>>
    arr5 createFunctionBlockEntry
  where
    createFunctionBlockEntry a b c d e =
        FunctionBlockEntry <$> pure a
                           <*> pure b
                           <*> pure c
                           <*> pure d
                           <*> e

getFunctionBlockIO :: ArrowXml a => a XmlTree (Either String FunctionBlockIO)
getFunctionBlockIO =
    getBlock "Input" DirectionInput `orElse` getBlock "Output" DirectionOutput
  where
    getBlock str dir =
        atTag str >>>
        getAttrValue "Name" &&&
        (atTag "Position" >>> getCoord) &&& (getTextAt "IsType" >>^ getType) >>>
        arr3 (createFunctionBlockIO dir)
    createFunctionBlockIO dir a b c = FunctionBlockIO dir <$> pure a <*> b <*> c
    getType str =
        case str of
            "Event" -> Right FunctionBlockIOEvent
            "Data" -> Right FunctionBlockIOData
            _ -> Left ("Unknown FunctionBlock direction: " <> str)
    getTextAt tag = atTag tag >>> getChildren >>> isText >>> getText
    getCoord = getTextAt "X" &&& getTextAt "Y" >>> createFloat

getCompositeFunctionBlock :: ArrowXml a => a XmlTree (Either String CompositeFunctionBlock)
getCompositeFunctionBlock =
    atTag "FBType" >>>
    getFunctionBlockDescription &&&
    getInterfaceList &&&
    (atTag "FBNetwork" >>>
     getList getFunctionBlock &&&
     getList getFunctionBlockIO &&&
     getListAtElem (getConnection EventConnection) "EventConnections" &&&
     getListAtElem (getConnection DataConnection) "DataConnections") >>>
    arr6 createCompositeFunctionBlock
  where
    createCompositeFunctionBlock a b c d e f =
        CompositeFunctionBlock <$> pure a
                               <*> b
                               <*> sequence c
                               <*> sequence d
                               <*> pure e
                               <*> pure f

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
readBlock f path =
    runX (readDocument xmlOptions path >>> f)

readBasicFunctionBlock :: FilePath -> IO (Either String [BasicFunctionBlock])
readBasicFunctionBlock = fmap sequence . readBlock getBasicFunctionBlock

readCompositeFunctionBlock :: FilePath -> IO (Either String [CompositeFunctionBlock])
readCompositeFunctionBlock = fmap sequence . readBlock getCompositeFunctionBlock
