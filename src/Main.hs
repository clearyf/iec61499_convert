{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import BasePrelude

import Text.XML.HaXml
import Text.XML.HaXml.Pretty

main :: IO ()
main = do xml <- readFile "examples/iec61449/minMax.xml"
          putStrLn (show (document (xmlParse "minMax" xml)))
