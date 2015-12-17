module ParseGuardSpec where

import           BasePrelude
import           Data.Set (Set)
import qualified Data.Set as Set
import           ParseGuard
import           ParseSt
import           Test.Hspec (it, shouldBe)
import           Test.Hspec.Core.Spec (Spec)

spec :: Spec
spec = do
    it
        "Simple event"
        (parseGuard events "BUTTON" `shouldBe`
         Just (Guard (Just "BUTTON") Nothing))
    it
        "Only condition"
        (parseGuard events "NOT button" `shouldBe`
         Just
             (Guard
                  Nothing
                  (Just (StMonoOp StNot (StLValue (SimpleLValue "button"))))))
    it
        "Event + condition 1"
        (parseGuard events "BUTTON AND NOT button" `shouldBe`
         Just
             (Guard
                  (Just "BUTTON")
                  (Just (StMonoOp StNot (StLValue (SimpleLValue "button"))))))
    it
        "Event + condition 2"
        (parseGuard events "BUTTON AND !(b1 OR b2) AND b3" `shouldBe`
         Just
             (Guard
                  (Just "BUTTON")
                  (Just (StBinaryOp StAnd (StMonoOp StNot (StSubValue (StBinaryOp StOr (StLValue (SimpleLValue "b1")) (StLValue (SimpleLValue "b2"))))) (StLValue (SimpleLValue "b3"))))))
    it
        "Event + condition 3"
        (parseGuard events "BLAH AND (((a & b) | c) & d) | e" `shouldBe`
         Just
             (Guard
                  (Just "BLAH")
                  (Just (StBinaryOp StOr (StSubValue (StBinaryOp StAnd (StSubValue (StBinaryOp StOr (StSubValue (StBinaryOp StAnd (StLValue (SimpleLValue "a")) (StLValue (SimpleLValue "b")))) (StLValue (SimpleLValue "c")))) (StLValue (SimpleLValue "d")))) (StLValue (SimpleLValue "e"))))))
    it
        "Event AND (variable = value)"
        (parseGuard events "BUTTON AND (setpoint = value)" `shouldBe`
         Just
             (Guard
                  (Just "BUTTON")
                  (Just (StSubValue (StBinaryOp StEquals (StLValue (SimpleLValue "setpoint")) (StLValue (SimpleLValue "value")))))))

events :: Set String
events = Set.fromList ["BUTTON", "BLAH"]
