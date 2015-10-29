{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import BasePrelude
import Test.Hspec (hspec, describe, it, shouldBe, runIO)
import Test.Hspec.Core.Spec (Spec)
import ParseIec61499

main :: IO ()
main = hspec tests

tests :: Spec
tests = do describe "Parser" testParser

testParser :: Spec
testParser =
  do toggleFunctionBlockFromFile <- runIO (readFunctionBlock toggleFile)
     it "Parse Function Block"
        (toggleFunctionBlockFromFile `shouldBe` toggleFunctionBlock)

toggleFile :: FilePath
toggleFile = "examples/iec61499/toggle_fb.xml"

toggleFunctionBlock :: [FunctionBlock]
toggleFunctionBlock =
  [FunctionBlock {interfaceList =
                    InterfaceList {eventInputs =
                                     [Event {eventName = "Toggle"
                                            ,eventComment = "Normal Execution Request"
                                            ,eventVariables = []}]
                                  ,eventOutputs =
                                     [Event {eventName = "Update"
                                            ,eventComment = "Execution Confirmation"
                                            ,eventVariables =
                                               ["Value"]}]
                                  ,inputVariables = []
                                  ,outputVariables =
                                     [Variable {variableName = "Value"
                                               ,variableType = IECBool
                                               ,variableComment = "Output event qualifier"}]}
                 ,basicFb =
                    BasicFunctionBlock {bfbElements =
                                          [ECCState (ECState {ecStateName = "START"
                                                             ,ecStateComment = "Initial State"
                                                             ,ecStateActions = []})
                                          ,ECCState (ECState {ecStateName = "On"
                                                             ,ecStateComment = "Normal execution"
                                                             ,ecStateActions =
                                                                [ECAction {ecActionAlgorithm = "TurnOn"
                                                                          ,ecActionOutput = "Update"}]})
                                          ,ECCState (ECState {ecStateName = "Off"
                                                             ,ecStateComment = ""
                                                             ,ecStateActions =
                                                                [ECAction {ecActionAlgorithm = "TurnOff"
                                                                          ,ecActionOutput = "Update"}]})
                                          ,ECCTransition
                                             (ECTransition {ecTransitionSource = "START"
                                                           ,ecTransitionDestination = "Off"
                                                           ,ecTransitionCondition = "1"})
                                          ,ECCTransition
                                             (ECTransition {ecTransitionSource = "Off"
                                                           ,ecTransitionDestination = "On"
                                                           ,ecTransitionCondition = "Toggle"})
                                          ,ECCTransition
                                             (ECTransition {ecTransitionSource = "On"
                                                           ,ecTransitionDestination = "Off"
                                                           ,ecTransitionCondition = "Toggle"})]
                                       ,bfbAlgorithms =
                                          [ECAlgorithm {ecAlgorithmName = "TurnOn"
                                                       ,ecAlgorithmComment = "Normally executed algorithm"
                                                       ,ecAlgorithmStText = "Value:=TRUE;"}
                                          ,ECAlgorithm {ecAlgorithmName = "TurnOff"
                                                       ,ecAlgorithmComment = ""
                                                       ,ecAlgorithmStText = "Value:=FALSE;"}]}}]
