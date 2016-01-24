module Main where

import BasePrelude
import ParseIec61499 (readFunctionBlock)
import FbToUppaal (fbToUppaalModel)
import OutputUppaal (outputUppaalToFile)

convertFile :: FilePath -> FilePath -> IO ()
convertFile inputPath outputPath = do
    putStrLn
        ("Converting: " <> inputPath <> ", writing output to: " <> outputPath)
    (contents:_) <- readFunctionBlock inputPath
    _ <- outputUppaalToFile outputPath (fbToUppaalModel contents)
    pure ()

showHelp :: IO ()
showHelp = putStrLn "Require 2 arguments, input file & output file"

main :: IO ()
main = do
    arguments <- getArgs
    -- Using a proper argument parser would be overkill for this...
    if length arguments == 2
        then convertFile (arguments !! 0) (arguments !! 1)
        else showHelp
