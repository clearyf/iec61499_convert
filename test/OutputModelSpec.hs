module OutputModelSpec where

import BasePrelude
import Test.Hspec (it, shouldBe, runIO)
import Test.Hspec.Core.Spec (Spec)
import OutputUppaal
import UppaalModel

spec :: Spec
spec = do
    outputModel <- runIO (outputUppaal uppaalModel)
    modelFromXml <- runIO (readFile "examples/uppaal/noheaderToggle.xml")
    it "toggle model" (outputModel `shouldBe` [modelFromXml])