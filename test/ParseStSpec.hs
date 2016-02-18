module ParseStSpec where

import BasePrelude
import Test.Hspec (it, shouldBe)
import Test.Hspec.Core.Spec (Spec)
import ParseSt
import StTypes
import Data.List.NonEmpty (NonEmpty(..))

spec :: Spec
spec = do
  it "Empty Statements" $ do
      parseSt "" " ; ; ;" `shouldBe` Right []
      parseSt "" " ; ; ; ;" `shouldBe` Right []
  it "Simple Assigment" $ do
      parseSt "" "VARIABLE:=FALSE;" `shouldBe`
        Right [Assignment (SimpleLValue "VARIABLE") (StBool False)]
      parseSt "" "IFFY := 2 ** 3;" `shouldBe`
        Right [Assignment (SimpleLValue "IFFY") (StBinaryOp StExp (StInt 2) (StInt 3))]
      parseSt "" "Value := 2 > 3;" `shouldBe`
        Right [Assignment (SimpleLValue "Value") (StBinaryOp StGreaterThan (StInt 2) (StInt 3))]
      parseSt "" "Value := 2 < 3;" `shouldBe`
        Right [Assignment (SimpleLValue "Value") (StBinaryOp StLessThan (StInt 2) (StInt 3))]
      parseSt "" "Value := 2 <> 3;" `shouldBe`
        Right [Assignment (SimpleLValue "Value") (StBinaryOp StNotEquals (StInt 2) (StInt 3))]
      parseSt "" "Value := -303;" `shouldBe`
        Right [Assignment (SimpleLValue "Value") (StMonoOp StNegate (StInt 303))]
      parseSt "" "Value := -0.333;" `shouldBe`
        Right [Assignment (SimpleLValue "Value") (StMonoOp StNegate (StFloat 0.333))]
      parseSt "" "Value := blah;" `shouldBe`
        Right [Assignment (SimpleLValue "Value") (StLValue (SimpleLValue "blah"))]
      parseSt "" "Value := TRUE;\ni := 3;\n" `shouldBe`
        Right
        [ Assignment (SimpleLValue "Value") (StBool True)
        , Assignment (SimpleLValue "i") (StInt 3)]
  it "Char/Strings" $ do
    parseSt "" "a := 'b';" `shouldBe` Right [Assignment (SimpleLValue "a") (StChar 'b')]
  it "Time" $ do
    parseSt "" "a := t#11h22m30s435ms;" `shouldBe`
      Right [Assignment (SimpleLValue "a") (StTime 40950435)]
  it "Arrays" $ do
      parseSt "" "value[i + 1] := 20 + i;" `shouldBe`
        Right [Assignment
               (ArrayLValue
                "value"
                (StBinaryOp StAddition (StLValue (SimpleLValue "i")) (StInt 1)))
               (StBinaryOp StAddition (StInt 20) (StLValue (SimpleLValue "i")))]
      parseSt "" "value[i + i] := 20 + arr[i + value[i - 1]];" `shouldBe`
        Right
        [Assignment (ArrayLValue "value" (StBinaryOp StAddition (StLValue (SimpleLValue "i")) (StLValue (SimpleLValue "i")))) (StBinaryOp StAddition (StInt 20) (StLValue (ArrayLValue "arr" (StBinaryOp StAddition (StLValue (SimpleLValue "i")) (StLValue (ArrayLValue "value" (StBinaryOp StSubtract (StLValue (SimpleLValue "i")) (StInt 1))))))))]
  it "Var declaration" $ do
      parseSt "" "VAR blah : STRING[10]; END_VAR;" `shouldBe`
        Right [ Declaration "blah" (IECString 10)]
      parseSt "" "VAR llh : INT; llt : REAL; END_VAR; llh := 5;" `shouldBe`
        Right [ Declaration "llh" (IECInt Sixteen) , Declaration "llt" IECReal , Assignment (SimpleLValue "llh") (StInt 5)]
      parseSt "" "VAR blah : UDINT; END_VAR ; blah := 10;" `shouldBe` Right [ Declaration "blah" (IECUInt ThirtyTwo) , Assignment (SimpleLValue "blah") (StInt 10)]
      parseSt "" "VAR blah : INT[10,10,10]; END_VAR;" `shouldBe` Right [Declaration "blah" (IECArray (10 :| [10,10]) (IECInt Sixteen))]
  it "FOR statements" $ do
    parseSt "" "VAR a : INT; END_VAR; FOR blah := 10 TO 0 BY -1 DO a := blah; END_FOR;" `shouldBe` Right [ Declaration "a" (IECInt Sixteen) , For "blah" 10 0 (Just (-1)) [Assignment (SimpleLValue "a") (StLValue (SimpleLValue "blah"))]]
    parseSt "" "FOR blah := 0 TO 10 DO a := blah; END_FOR;" `shouldBe` Right [ For "blah" 0 10 Nothing [Assignment (SimpleLValue "a") (StLValue (SimpleLValue "blah"))]]
  it "IF statements" $ do
      parseSt "" "IF value = 1 THEN out := TRUE; ELSE IF value = 0 THEN out := FALSE; ELSE error := TRUE; END_IF; END_IF;" `shouldBe` Right [ IfElse (StBinaryOp StEquals (StLValue (SimpleLValue "value")) (StInt 1)) [Assignment (SimpleLValue "out") (StBool True)] [ IfElse (StBinaryOp StEquals (StLValue (SimpleLValue "value")) (StInt 0)) [Assignment (SimpleLValue "out") (StBool False)] [Assignment (SimpleLValue "error") (StBool True)]]]
  it "RETURN statement" $ do
    parseSt "" "RETURN;" `shouldBe` Right [Return]
  it "BREAK statement" $ do
    parseSt "" "EXIT;" `shouldBe` Right [Break]
  it "WHILE statements" $ do
    parseSt "" "WHILE a < 4 DO a := a + 1; END_WHILE;" `shouldBe`
      Right [While
             (StBinaryOp StLessThan (StLValue (SimpleLValue "a")) (StInt 4))
             [Assignment
              (SimpleLValue "a")
              (StBinaryOp StAddition (StLValue (SimpleLValue "a")) (StInt 1))]]
  it "REPEAT statments" $ do
    parseSt "" "REPEAT a := a + 1; UNTIL a >= 4 END_REPEAT;" `shouldBe`
      Right [Repeat
             [Assignment
              (SimpleLValue "a")
              (StBinaryOp StAddition (StLValue (SimpleLValue "a")) (StInt 1))
              ]
             (StBinaryOp StGreaterThanEquals (StLValue (SimpleLValue "a")) (StInt 4))
             ]
  it "Parens" $ do
    parseSt "" "a := 1 * (2 + 3);" `shouldBe`
      Right [Assignment
             (SimpleLValue "a")
             (StBinaryOp StMultiply (StInt 1) (StSubValue (StBinaryOp StAddition (StInt 2) (StInt 3))))]
  it "Case" $ do
    parseSt "" "CASE a OF 1,2-3,4,5,6,7-10: a := 2; 11: a := 3; ELSE a := 4; END_CASE;" `shouldBe`
      Right
      [Case
       (StLValue (SimpleLValue "a"))
       [(CaseInt 1 :| [CaseRange 2 3,CaseInt 4,CaseInt 5,CaseInt 6,CaseRange 7 10],
         [Assignment (SimpleLValue "a") (StInt 2)])
       ,(CaseInt 11 :| [],
         [Assignment (SimpleLValue "a") (StInt 3)])]
       [Assignment (SimpleLValue "a") (StInt 4)]]
  it "Functions" $ do
      parseSt "" "blah := max(2 - 32, abs(ao) * 3);" `shouldBe`
       Right
       [Assignment
        (SimpleLValue "blah")
        (StFunc "max" [StBinaryOp StSubtract (StInt 2) (StInt 32)
                      ,StBinaryOp StMultiply
                       (StFunc "abs" [StLValue (SimpleLValue "ao")])
                       (StInt 3)])]
  it "svVerifyBoolLogic" $
    do parseSt ""
           "IF OOB THEN OOB := FALSE; ELSE EOOB := FALSE;END_IF;IF tDelay <> 0 THEN tDelay := 0;ELSE STOP := FALSE; END_IF;" `shouldBe`
           Right
               [ IfElse
                     (StLValue (SimpleLValue "OOB"))
                     [Assignment (SimpleLValue "OOB") (StBool False)]
                     [Assignment (SimpleLValue "EOOB") (StBool False)]
               , IfElse
                     (StBinaryOp
                          StNotEquals
                          (StLValue (SimpleLValue "tDelay"))
                          (StInt 0))
                     [Assignment (SimpleLValue "tDelay") (StInt 0)]
                     [Assignment (SimpleLValue "STOP") (StBool False)]]
