{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import BasePrelude hiding (orElse)
import ParseIec61499 (readFunctionBlock, FunctionBlock)

test :: IO [FunctionBlock]
test = do let path = "examples/iec61499/toggle_fb.xml"
          readFunctionBlock path

main :: IO ()
main = do result <- test
          print result
