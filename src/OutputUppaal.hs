{-# LANGUAGE FlexibleContexts #-}

module OutputUppaal (outputUppaal, outputUppaalToFile) where

import BasePrelude
import UppaalTypes
import Text.XML.HXT.Core
       (ArrowXml(..), IOSLA(..), XIOState, XmlTree, addDoctypeDecl,
        addXmlPi, addXmlPiEncoding, aelem, mkelem, runX, sattr, selem, txt,
        withIndent, withXmlPi, writeDocument, writeDocumentToString, yes)

showStateId :: StateId -> String
showStateId (StateId i) = "id" <> show i

--------------------------------------------------------------------------------

-- This file is divided into three sections, as the uppaal xml file is
-- basically subdivided into three sections, the global declarations,
-- the template declarations (locations & transitions) and finally the
-- system declaration.

uppaalDtd :: String
uppaalDtd = "http://www.it.uu.se/research/group/darts/uppaal/flat-1_1.dtd"

uppaalDecl :: String
uppaalDecl = "-//Uppaal Team//DTD Flat System 1.1//EN"

addDtd :: ArrowXml a => a XmlTree XmlTree
addDtd = addDoctypeDecl "nta" uppaalDecl uppaalDtd

outputModel :: IOSLA (XIOState ()) XmlTree c -> UppaalModel -> IO [c]
outputModel outputFunction um =
    runX
        (root [] [selem "nta" [sections]] >>>
         addDtd >>> addXmlPi >>> addXmlPiEncoding "utf-8" >>> outputFunction)
  where
    sections = globalDecl um <+> templateDecl um <+> systemDecl um

outputUppaalToFile :: String -> UppaalModel -> IO [XmlTree]
outputUppaalToFile name = outputModel (writeDocument [withXmlPi yes] name)

outputUppaal :: UppaalModel -> IO [String]
outputUppaal = outputModel (writeDocumentToString [withIndent yes, withXmlPi yes])

--------------------------------------------------------------------------------
-- The global declaration first.

inputChannels :: UppaalModel -> [String]
inputChannels um = map (\ (UppaalChan c) -> "chan " <> c) (modelInputEvents um)

outputChannels :: UppaalModel -> [String]
outputChannels um = map (\ (UppaalChan c) -> "chan " <> c) (modelOutputEvents um)

parameters :: (t -> [UppaalVar]) -> t -> [String]
parameters f um = map (\ (UppaalVar t v) -> t <> " " <> v) (f um)

createGlobalDecl :: UppaalModel -> String
createGlobalDecl um =
    "// Global declarations\n" <>
    foldMap
        (<> ";\n")
        (inputChannels um <> outputChannels um <> parameters modelInputVars um <>
         parameters modelOutputVars um) <>
    modelDeclarations um

createLocalDeclarations :: UppaalModel -> String
createLocalDeclarations um =
    "// Local declarations\n" <>
    foldMap (<> ";\n") (parameters modelInternalVars um)

-- The global declarations consist of the input/output events and
-- input/output values.
globalDecl :: ArrowXml a  => UppaalModel -> a n XmlTree
globalDecl um = selem "declaration" [txt (createGlobalDecl um)]

--------------------------------------------------------------------------------
-- The template declaration second.

-- TODO Fix coordinates for name declaration.
templateDecl :: ArrowXml a => UppaalModel -> a n XmlTree
templateDecl um =
    selem
        "template"
        ([ mkelem "name" [sattr "x" "0", sattr "y" "0"] [txt (modelName um)]
         , selem "declaration" [txt (createLocalDeclarations um)]] <>
         map makeLocationDecl (modelLocations um) <>
         makeInitialLocation <>
         map makeTransitionDecl (modelTransitions um))

makeLocationDecl :: ArrowXml a => Location -> a n XmlTree
makeLocationDecl l =
    case l of
        Location s -> mk s mempty
        UrgentLocation s -> mk s [eelem "urgent"]
  where
    mk (AState n i coord) extra =
        mkelem
            "location"
            [ sattr "id" (showStateId i)
            , sattr "x" (show (realPart coord))
            , sattr "y" (show (imagPart coord))]
            ([ mkelem
                   "name"
                   [ sattr "x" (show (realPart coord + 10))
                   , sattr "y" (show (imagPart coord + 10))]
                   [txt n]] <>
             extra)

makeTransitionDecl :: ArrowXml a => Transition -> a n XmlTree
makeTransitionDecl (Transition src dest sync guard' assignment) =
    selem
        "transition"
        ([ aelem "source" [sattr "ref" (showStateId src)]
         , aelem "target" [sattr "ref" (showStateId dest)]] <>
         maybe mempty syncElem sync <>
         maybe mempty guardElem guard' <>
         maybe mempty assignmentElem assignment)
  where
    syncElem s =
        [ mkelem
              "label"
              [sattr "kind" "synchronisation", sattr "x" "0", sattr "y" "0"]
              [txt s]]
    guardElem g =
        [ mkelem
              "label"
              [sattr "kind" "guard", sattr "x" "0", sattr "y" "0"]
              [txt g]]
    assignmentElem u =
        [ mkelem
              "label"
              [sattr "kind" "assignment", sattr "x" "0", sattr "y" "0"]
              [txt u]]

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
