module FbToUppaalSpec where

import BasePrelude
import FbToUppaal
import ParseIec61499
import ParseSt
import Test.Hspec (it, shouldBe)
import Test.Hspec.Core.Spec (Spec)
import ToggleFunctionBlock
import UppaalModel

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
        [Assignment
         (SimpleLValue "blah")
         (StFunc
          "max"
          [StBinaryOp StAddition (StInt 10) (StFloat 10.1)
          ,StFloat 0.3313])]
output1 :: String
output1 = "void name()\n{\n\tblah = max(10 + 10.1, 0.3313);\n}\n\n"

input2 :: ECAlgorithm
input2 =
    ECAlgorithm
        "name"
        ""
        [Declaration "blah" (IECInt Eight)
        ,Assignment
         (SimpleLValue "blah")
         (StFunc
          "max"
          [StBinaryOp StAddition (StInt 10) (StFunc "min" [StInt 2, StInt 10])
          ,StFloat 0.0013])]
output2 :: String
output2  = "void name()\n{\n\tint[-128,127] blah;\n\tblah = max(10 + min(2, 10), 0.0013);\n}\n\n"
