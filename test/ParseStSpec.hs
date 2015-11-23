module ParseStSpec where

import BasePrelude
import Test.Hspec (it, shouldBe)
import Test.Hspec.Core.Spec (Spec)
import ParseSt

spec :: Spec
spec =
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
