module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

-- Import modules from your snake game to test
import Game.Core

-- Define your test cases
testInitialState :: Test
testInitialState = TestCase $ do
  assertEqual "Init state"
    initialState
    (Snake {screenSize = (10, 10), snakeDirection = Game.Core.Right, snakePosition = [(0, 0), (0, 1), (0, 2)]})

-- Group all tests together
tests :: Test
tests = TestList [
    TestLabel "Initial state" testInitialState
    -- Add more tests here
  ]

-- Main function to run tests
main :: IO ()
main = do
  counts' <- runTestTT tests
  if errors counts' + failures counts' > 0
    then exitFailure
    else exitSuccess