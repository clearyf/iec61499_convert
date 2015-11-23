module Main where

import BasePrelude hiding (orElse)
import ParseIec61499 (readFunctionBlock, FunctionBlock)
import FbToUppaal
import OutputUppaal
import Text.XML.HXT.Core

test :: IO [FunctionBlock]
test = do
    let path = "examples/iec61499/toggle_fb.xml"
    readFunctionBlock path

theStdout :: Monoid m => m
theStdout = mempty

doFile :: FilePath -> IO [XmlTree]
doFile path = do
    (contents:_) <- readFunctionBlock path
    outputUppaalToFile (fbToUppaalModel contents) theStdout

main :: IO ()
main = do
    arguments <- getArgs
    traverse_ doFile arguments
