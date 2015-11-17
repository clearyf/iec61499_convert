module OutputUppaal
       (UppaalModel(..), AState(..), UppaalChan(..), UppaalVar(..),
        Location(..), StateId(..), Transition(..), outputUppaal)
       where

import           BasePrelude
import           Text.XML.HXT.Core

data UppaalModel = UppaalModel
    { modelName :: String
    , modelInputEvents :: [UppaalChan]
    , modelOutputEvents :: [UppaalChan]
    , modelInputVars :: [UppaalVar]
    , modelOutputVars :: [UppaalVar]
    , modelLocations :: [Location]
    , modelTransitions :: [Transition]
    , modelDeclarations :: [String]
    } deriving (Show,Eq)

data UppaalVar = UppaalVar String String deriving (Show,Eq)
newtype UppaalChan = UppaalChan String deriving (Show,Eq)

newtype StateId  = StateId  String deriving (Show,Eq)

data Location
    = Location AState
    | UrgentLocation AState
    deriving (Show,Eq)

data Transition = Transition
    { transitionSrc :: StateId
    , transitionDest :: StateId
    , transitionSync :: String
    , transitionUpdate :: String
    } deriving (Show,Eq)

data AState = AState
    { stateName :: String
    , stateId :: StateId
    } deriving (Show,Eq)

--------------------------------------------------------------------------------

-- This file is divided into three sections, as the uppaal xml file is
-- basically subdivided into three sections, the global declarations,
-- the template declarations (locations & transitions) and finally the
-- system declaration.

outputUppaal :: UppaalModel -> IO [String]
outputUppaal um =
    runX
        (root [] [mkelem "nta" [] [sections]] >>>
         writeDocumentToString [withIndent yes])
  where
    sections = globalDecl um <+> templateDecl um <+> systemDecl um

--------------------------------------------------------------------------------
-- The global declaration first.

inputChannels :: UppaalModel -> [String]
inputChannels um = map (\ (UppaalChan c) -> "chan " <> c) (modelInputEvents um)

outputChannels :: UppaalModel -> [String]
outputChannels um = map (\ (UppaalChan c) -> "chan " <> c) (modelOutputEvents um)

inputParameters :: UppaalModel -> [String]
inputParameters um = map (\ (UppaalVar t v) -> t <> " " <> v) (modelInputVars um)

outputParameters :: UppaalModel -> [String]
outputParameters um = map (\ (UppaalVar t v) -> t <> " " <> v) (modelOutputVars um)

createGlobalDecl :: UppaalModel -> String
createGlobalDecl um =
    "// Global declarations\n" <>
    (foldMap
         (<> ";\n")
         (inputChannels um <> outputChannels um <> inputParameters um <>
          outputParameters um)) <>
    mconcat (modelDeclarations um)

-- The global declarations consist of the input/output events and
-- input/output values.
globalDecl :: ArrowXml a  => UppaalModel -> a n XmlTree
globalDecl um = selem "declarations" [txt (createGlobalDecl um)]

--------------------------------------------------------------------------------
-- The template declaration second.

-- TODO Fix coordinates for name declaration.
templateDecl :: ArrowXml a => UppaalModel -> a n XmlTree
templateDecl um =
    selem
        "template"
        ([ mkelem "name" [sattr "x" "0", sattr "y" "0"] [txt (modelName um)]
         , selem "declarations" [txt "// Declarations\n"]] <>
         (map makeLocationDecl (modelLocations um)) <>
         makeInitialLocation <>
         (map makeTransitionDecl (modelTransitions um)))

makeLocationDecl :: ArrowXml a => Location -> a n XmlTree
makeLocationDecl (Location (AState n (StateId i))) =
    mkelem
        "location"
        [sattr "id" i, sattr "x" "0", sattr "y" "0"]
        [mkelem "name" [sattr "x" "0", sattr "y" "0"] [txt n]]
makeLocationDecl (UrgentLocation (AState n (StateId i))) =
    mkelem
        "location"
        [sattr "id" i, sattr "x" "0", sattr "y" "0"]
        [mkelem "name" [sattr "x" "0", sattr "y" "0"] [txt n], eelem "urgent"]

makeTransitionDecl :: ArrowXml a => Transition -> a n XmlTree
makeTransitionDecl (Transition (StateId src) (StateId dest) sync update) =
    selem
        "transition"
        [ aelem "source" [sattr "ref" src]
        , aelem "target" [sattr "ref" dest]
        , mkelem
              "label"
              [sattr "kind" "synchronisation", sattr "x" "0", sattr "y" "0"]
              [txt sync]
        , mkelem
              "label"
              [sattr "kind" "update", sattr "x" "0", sattr "y" "0"]
              [txt update]]

-- TODO it's not quite clear how the initial state of the system is
-- defined; either it's the first state listed in the structure, or
-- it's the one called "START".  Take the first state...
makeInitialLocation :: ArrowXml a => [a n XmlTree]
makeInitialLocation = [aelem "init" [sattr "ref" "id0"]]

--------------------------------------------------------------------------------
-- The system declaration last.

createSystem :: UppaalModel -> String
createSystem um =
    "// System setup\n" <> systemName <> "blk = " <> systemName <>
    "();\nsystem " <>
    systemName <>
    "blk;\n"
  where
    systemName = modelName um

systemDecl :: ArrowXml a => UppaalModel -> a n XmlTree
systemDecl um = selem "system" [txt (createSystem um)]
