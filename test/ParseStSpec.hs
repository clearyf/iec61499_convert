module ParseStSpec where

import BasePrelude
import Test.Hspec (it, shouldBe)
import Test.Hspec.Core.Spec (Spec)
import ParseSt
import Iec61131

spec :: Spec
spec = do
    it "Empty Statements"
        (do parseSt " ; ; ;" `shouldBe` Right []
            parseSt " ; ; ; ;" `shouldBe` Right [])
    it
        "Simple Assigment"
        (do parseSt "Value:=FALSE;" `shouldBe`
                Right [Assignment "Value" [StBool False]]
            parseSt "Value := -303;" `shouldBe`
                Right [Assignment "Value" [StOp "-", StInt 303]]
            parseSt "Value := -0.333;" `shouldBe`
                Right [Assignment "Value" [StOp "-", StFloat 0.333]]
            parseSt "Value := blah;" `shouldBe`
                Right [Assignment "Value" [StVar "blah"]]
            parseSt "Value := TRUE;\ni := 3;\n" `shouldBe`
                Right
                    [Assignment "Value" [StBool True], Assignment "i" [StInt 3]])
    it
        "Var declaration"
        (do parseSt "VAR; llh : INT; llt : REAL; END_VAR; llh := 5;" `shouldBe`
                Right
                    [ Declaration "llh" (IECInt Sixteen)
                    , Declaration "llt" IECReal
                    , Assignment "llh" [StInt 5]]
            parseSt "VAR; blah : UDINT; END_VAR ; blah := 10;" `shouldBe`
                Right
                    [ Declaration "blah" (IECUInt ThirtyTwo)
                    , Assignment "blah" [StInt 10]])
    it
        "IF statements"
        (do parseSt
                "IF value = 1 THEN out := TRUE; ELSE IF value = 0 THEN out := FALSE; ELSE error := TRUE; END_IF; END_IF;" `shouldBe`
                Right
                    [ IfElse
                          [StVar "value", StOp "=", StInt 1]
                          [Assignment "out" [StBool True]]
                          [ IfElse
                                [StVar "value", StOp "=", StInt 0]
                                [Assignment "out" [StBool False]]
                                [Assignment "error" [StBool True]]]])
    it
        "Functions"
        (do parseSt "blah := max(2 - 32, abs(ao) * 3);" `shouldBe`
                Right
                    [ Assignment
                          "blah"
                          [ StFunc
                                "max"
                                [ [StInt 2, StOp "-", StInt 32]
                                , [ StFunc "abs" [[StVar "ao"]]
                                  , StOp "*"
                                  , StInt 3]]]])
