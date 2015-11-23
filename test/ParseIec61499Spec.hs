module ParseIec61499Spec where

import BasePrelude
import Test.Hspec (it, shouldBe, runIO)
import Test.Hspec.Core.Spec (Spec)
import ParseIec61499
import ToggleFunctionBlock

spec :: Spec
spec = do
    toggleFunctionBlockFromFile <- runIO (readFunctionBlock toggleFile)
    it
        "Function Block"
        (toggleFunctionBlockFromFile `shouldBe` toggleFunctionBlock)

toggleFile :: FilePath
toggleFile = "examples/iec61499/toggle_fb.xml"
