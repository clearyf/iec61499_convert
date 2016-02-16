module OutputModelSpec where

import BasePrelude
import Test.Hspec (it, shouldBe, runIO)
import Test.Hspec.Core.Spec (Spec)
import OutputUppaal
import UppaalModel

spec :: Spec
spec = do
    model <- runIO (outputUppaal uppaalModel)
    modelFromXml <- runIO (readFile "examples/uppaal/toggle.xml")
    it "toggle model" (model `shouldBe` [modelFromXml])
