module FbToUppaalSpec where

import BasePrelude
import Test.Hspec (it, shouldBe)
import Test.Hspec.Core.Spec (Spec)
import FbToUppaal
import UppaalModel
import ToggleFunctionBlock
import ParseSt
import ParseIec61499

spec :: Spec
spec = do
    it
        "Toggle system"
        (fmap fbToUppaalModel toggleFunctionBlock `shouldBe` [uppaalModel])
    it "Algorithm output" $ do anAlgorithm input1 `shouldBe` output1
                               anAlgorithm input2 `shouldBe` output2

input1 :: ECAlgorithm
input1 =
    ECAlgorithm
        "name"
        ""
        [ Assignment
              "blah"
              [ StFunc
                    "max"
                    [[StInt 10, StOp "+", StFloat 10.1], [StFloat 0.3313]]]]
output1 :: String
output1 = "void name()\n{\n\tblah = max(10 + 10.1, 0.3313);\n}\n"

input2 :: ECAlgorithm
input2 =
    ECAlgorithm
        "name"
        ""
        [ Assignment
              "blah"
              [ StFunc
                    "max"
                    [ [StInt 10, StOp "+", StFunc "min" [[StInt 2], [StInt 10]]]
                    , [StFloat 0.3313]]]]
output2 :: String
output2  = "void name()\n{\n\tblah = max(10 + min(2, 10), 0.3313);\n}\n"
