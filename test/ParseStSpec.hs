module ParseStSpec where

import BasePrelude
import Test.Hspec (it, shouldBe)
import Test.Hspec.Core.Spec (Spec)
import ParseSt
import Data.List.NonEmpty (NonEmpty(..))

spec :: Spec
spec = do
  it "Empty Statements" $ do
      parseSt " ; ; ;" `shouldBe` Right []
      parseSt " ; ; ; ;" `shouldBe` Right []
  it "Simple Assigment" $ do
      parseSt "Value:=FALSE;" `shouldBe`
        Right [Assignment (SimpleLValue "Value") (StBool False :| [])]
      parseSt "Value := 2 ** 3;" `shouldBe`
        Right [Assignment (SimpleLValue "Value") (StInt 2 :| [StOp "**", StInt 3])]
      parseSt "Value := -303;" `shouldBe`
        Right [Assignment (SimpleLValue "Value") (StOp "-" :| [StInt 303])]
      parseSt "Value := -0.333;" `shouldBe`
        Right [Assignment (SimpleLValue "Value") (StOp "-" :| [StFloat 0.333])]
      parseSt "Value := blah;" `shouldBe`
        Right [Assignment (SimpleLValue "Value") (StLValue (SimpleLValue "blah") :| [])]
      parseSt "Value := TRUE;\ni := 3;\n" `shouldBe`
        Right
        [ Assignment (SimpleLValue "Value") (StBool True :| [])
        , Assignment (SimpleLValue "i") (StInt 3 :| [])]
  it "Arrays" $ do
      parseSt "value[i + 1] := 20 + i;" `shouldBe`
        Right [ Assignment
                (ArrayLValue
                 "value"
                 (StLValue (SimpleLValue "i") :| [StOp "+", StInt 1]))
                (StInt 20 :| [StOp "+", StLValue (SimpleLValue "i")])]
      parseSt "value[i + i] := 20 + arr[i + value[i - 1]];" `shouldBe`
        Right
        [Assignment
         (ArrayLValue
          "value"
          (StLValue (SimpleLValue "i") :|
           [StOp "+",StLValue (SimpleLValue "i")]))
         (StInt 20 :| [ StOp "+"
                      , StLValue
                        (ArrayLValue
                         "arr"
                         (StLValue (SimpleLValue "i") :|
                          [ StOp "+"
                          , StLValue (ArrayLValue
                                      "value"
                                      (StLValue (SimpleLValue "i") :|
                                       [StOp "-" , StInt 1]))]))])]
  it "Var declaration" $ do
      parseSt "VAR llh : INT; llt : REAL; END_VAR; llh := 5;" `shouldBe`
        Right [ Declaration "llh" (IECInt Sixteen) , Declaration "llt" IECReal , Assignment (SimpleLValue "llh") (StInt 5 :| [])]
      parseSt "VAR blah : UDINT; END_VAR ; blah := 10;" `shouldBe` Right [ Declaration "blah" (IECUInt ThirtyTwo) , Assignment (SimpleLValue "blah") (StInt 10 :| [])]
      parseSt "VAR blah : INT[10,10,10]; END_VAR;" `shouldBe` Right [Declaration "blah" (IECArray (10 :| [10,10]) (IECInt Sixteen))]
  it "FOR statements" $ do
      parseSt "VAR a : INT; END_VAR; FOR blah := 0 TO 10 BY 1 DO a := blah; END_FOR;" `shouldBe` Right [ Declaration "a" (IECInt Sixteen) , For "blah" 0 10 1 [Assignment (SimpleLValue "a") (StLValue (SimpleLValue "blah") :| [])]]
  it "IF statements" $ do
      parseSt "IF value = 1 THEN out := TRUE; ELSE IF value = 0 THEN out := FALSE; ELSE error := TRUE; END_IF; END_IF;" `shouldBe` Right [ IfElse (StLValue (SimpleLValue "value") :| [StOp "=", StInt 1] ) [Assignment (SimpleLValue "out") (StBool True :| [])] [ IfElse (StLValue (SimpleLValue "value") :| [StOp "=", StInt 0]) [Assignment (SimpleLValue "out") (StBool False :| [])] [Assignment (SimpleLValue "error") (StBool True :| [])]]]
  it "Functions" $ do
      parseSt "blah := max(2 - 32, abs(ao) * 3);" `shouldBe`
       Right
       [Assignment
        (SimpleLValue "blah")
        (StFunc "max" [StInt 2 :| [StOp "-",StInt 32]
                      ,StFunc
                       "abs"
                       [StLValue (SimpleLValue "ao") :| []]
                       :| [StOp "*",StInt 3]] :| [])]
