module FbToUppaalSpec where

import BasePrelude
import Test.Hspec (it, shouldBe)
import Test.Hspec.Core.Spec (Spec)
import FbToUppaal
import UppaalModel
import ToggleFunctionBlock

spec :: Spec
spec =
    it
        "Toggle system"
        (fmap fbToUppaalModel toggleFunctionBlock `shouldBe` [uppaalModel])
