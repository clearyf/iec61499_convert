module ParseIec61499Spec where

import BasePrelude
import Test.Hspec (it, shouldBe, runIO)
import Test.Hspec.Core.Spec (Spec)
import ParseIec61499
import ToggleFunctionBlock
import Demux8
import ButtonTimerLogic
import SvVerifyBoolLogic
import MinMax

spec :: Spec
spec = do
    checkFile buttonTimerLogicBlock "examples/iec61499/buttonTimerLogic.xml"
    checkFile demux8Block "examples/iec61499/demux8.xml"
    checkFile minMaxBlock "examples/iec61499/minMax.xml"
    checkFile svVerifyBoolLogicBlock "examples/iec61499/svVerifyBoolLogic.xml"
    checkFile toggleFunctionBlock "examples/iec61499/toggle_fb.xml"

checkFile :: [FunctionBlock] -> FilePath -> Spec
checkFile block filename = do
    fromFile <- runIO (readBasicFunctionBlock filename)
    it ("Function Block: " <> filename) (fromFile `shouldBe` block)
