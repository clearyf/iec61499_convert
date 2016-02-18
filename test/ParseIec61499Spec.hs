module ParseIec61499Spec where

import BasePrelude
import Test.Hspec (it, shouldBe, runIO)
import Test.Hspec.Core.Spec (Spec)
import ParseIec61499
import Iec61499Types
import ToggleFunctionBlock
import Demux8
import ButtonTimerLogic
import ButtonTimerComposite
import SvVerifyBoolLogic
import MinMax

spec :: Spec
spec = do
    checkFile buttonTimerLogicBlock "examples/iec61499/buttonTimerLogic.xml"
    checkFile demux8Block "examples/iec61499/demux8.xml"
    checkFile minMaxBlock "examples/iec61499/minMax.xml"
    checkFile svVerifyBoolLogicBlock "examples/iec61499/svVerifyBoolLogic.xml"
    checkFile toggleFunctionBlock "examples/iec61499/toggle_fb.xml"
    checkCFile buttonTimerCompositeBlock "examples/iec61499/ButtonTimer.fbt"

checkFile :: [BasicFunctionBlock] -> FilePath -> Spec
checkFile block filename = do
    fromFile <- runIO (readBasicFunctionBlock filename)
    it ("Function Block: " <> filename) (fromFile `shouldBe` Right block)

checkCFile :: [CompositeFunctionBlock] -> FilePath -> Spec
checkCFile block filename = do
    fromFile <- runIO (readCompositeFunctionBlock filename)
    it ("Function Block: " <> filename) (fromFile `shouldBe` Right block)
