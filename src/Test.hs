{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import BasePrelude
import Test.Hspec (hspec, describe, it, shouldBe)
import Test.Hspec.Core.Spec (Spec)

main :: IO ()
main = hspec tests

tests :: Spec
tests = do describe "Parser" testParser

testParser :: Spec
testParser = do it "Parse Number" (False `shouldBe` True)
