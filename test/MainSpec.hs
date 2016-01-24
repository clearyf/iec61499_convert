module MainSpec where

import BasePrelude
import FbToUppaal
import OutputUppaal
import ParseIec61499
import Test.Hspec (it, runIO, shouldBe)
import Test.Hspec.Core.Spec (Spec)

spec :: Spec
spec = do
    checkFile
        "examples/iec61499/buttonTimerLogic.xml"
        "examples/uppaal/buttonTimerLogic.xml"
    -- -- checkFile
    -- --     "examples/iec61499/clLux2Perc.xml"
    -- --     "examples/uppaal/noheaderToggle.xml"
    -- -- checkFile
    -- --     "examples/iec61499/compareLimitsReal.xml"
    -- --     "examples/uppaal/noheaderToggle.xml"
    -- -- checkFile
    -- --     "examples/iec61499/compareString.xml"
    -- --     "examples/uppaal/noheaderToggle.xml"
    checkFile
        "examples/iec61499/demux8.xml"
        "examples/uppaal/demux8.xml"
    -- -- checkFile
    -- --     "examples/iec61499/hcSetpointCalcLogic.xml"
    -- --     "examples/uppaal/noheaderToggle.xml"
    checkFile
        "examples/iec61499/HVglobalError.xml"
        "examples/uppaal/HVglobalError.xml"
    -- -- checkFile
    -- --     "examples/iec61499/minMax.xml"
    -- --     "examples/uppaal/noheaderToggle.xml"
    checkFile
        "examples/iec61499/registerLogic.xml"
        "examples/uppaal/registerLogic.xml"
    checkFile
        "examples/iec61499/RF_TRIG.xml"
        "examples/uppaal/RF_TRIG.xml"
    -- -- checkFile
    -- --     "examples/iec61499/segRoomIdLogic.xml"
    -- --     "examples/uppaal/noheaderToggle.xml"
    checkFile
        "examples/iec61499/svVerifyBoolLogic.xml"
        "examples/uppaal/svVerifyBoolLogic.xml"
    -- checkFile
    --     "examples/iec61499/toggle_fb.xml"
    --     "examples/uppaal/noheaderToggle.xml"
    -- -- checkFile
    -- --     "examples/iec61499/unitConvTemp.xml"
    -- --     "examples/uppaal/noheaderToggle.xml"

checkFile :: FilePath -> FilePath -> Spec
checkFile inPath outPath = do
    model <-
        runIO (readFunctionBlock inPath >>= outputUppaal . fbToUppaalModel . head >>= pure . head)
    uppaal <- runIO (readFile outPath)
    it inPath (model `shouldBe` uppaal)
