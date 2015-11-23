module ParseGuardSpec where

import           BasePrelude
import           Data.Set (Set)
import qualified Data.Set as Set
import           ParseGuard
import           Test.Hspec (it, shouldBe)
import           Test.Hspec.Core.Spec (Spec)

spec :: Spec
spec = do
    it
        "Simple event"
        (parseGuard events "BUTTON" `shouldBe`
         Right (Guard (Just "BUTTON") Nothing))
    it
        "Only condition"
        (parseGuard events "NOT button" `shouldBe`
         Right
             (Guard
                  Nothing
                  (Just (GuardSubCondition [GuardNot, GuardVariable "button"]))))
    it
        "Event + condition 1"
        (parseGuard events "BUTTON AND NOT button" `shouldBe`
         Right
             (Guard
                  (Just "BUTTON")
                  (Just (GuardSubCondition [GuardNot, GuardVariable "button"]))))
    it
        "Event + condition 2"
        (parseGuard events "BUTTON AND !(b1 OR b2) AND b3" `shouldBe`
         Right
             (Guard
                  (Just "BUTTON")
                  (Just
                       (GuardSubCondition
                            [ GuardNot
                            , GuardSubCondition
                                  [ GuardVariable "b1"
                                  , GuardOr
                                  , GuardVariable "b2"]
                            , GuardAnd
                            , GuardVariable "b3"]))))
    it
        "Event + condition 3"
        (parseGuard events "BLAH AND (((a & b) | c) & d) | e" `shouldBe`
         Right
             (Guard
                  (Just "BLAH")
                  (Just
                       (GuardSubCondition
                            [ GuardSubCondition
                                  [ GuardSubCondition
                                        [ GuardSubCondition
                                              [ GuardVariable "a"
                                              , GuardAnd
                                              , GuardVariable "b"]
                                        , GuardOr
                                        , GuardVariable "c"]
                                  , GuardAnd
                                  , GuardVariable "d"]
                            , GuardOr
                            , GuardVariable "e"]))))

events :: Set String
events = Set.fromList ["BUTTON", "BLAH"]
