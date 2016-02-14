module Main where

import BasePrelude
import ParseIec61499 (readBasicFunctionBlock)
import FbToUppaal (fbToUppaalModel)
import OutputUppaal (outputUppaalToFile)

convertFile :: FilePath -> FilePath -> IO ()
convertFile inputPath outputPath = do
    putStrLn
        ("Converting: " <> inputPath <> ", writing output to: " <> outputPath)
    contents <- readBasicFunctionBlock inputPath
    case contents of
        Left str -> putStrLn str
        Right block ->
            if null block
                then putStrLn "No functionblock parsed, but no error either!"
                else do
                    _ <-
                        outputUppaalToFile
                            outputPath
                            (fbToUppaalModel (head block))
                    pure ()
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
