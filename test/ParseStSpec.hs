module ParseStSpec where

import BasePrelude
import Test.Hspec (it, shouldBe)
import Test.Hspec.Core.Spec (Spec)
import ParseSt

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
                Right [Assignment "Value" [StInt (-303)]]
            parseSt "Value := blah;" `shouldBe`
                Right [Assignment "Value" [StVar "blah"]]
            parseSt "Value := TRUE;\ni := 3;\n" `shouldBe`
                Right
                    [Assignment "Value" [StBool True], Assignment "i" [StInt 3]])
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
