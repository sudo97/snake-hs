module Main where

-- Import modules from your snake game to test
import Game.Core
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

-- Define your test cases
testInitialState :: Test
testInitialState = TestCase $ do
  assertEqual
    "Init state"
    initialState
    (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 0), (0, 1), (0, 2)]})

testStep :: Test
testStep =
  TestList
    [ TestCase $ do
        assertEqual
          "Step"
          (step initialState)
          (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 1), (0, 2), (0, 3)]}),
      TestCase $ do
        assertEqual
          "Step twice"
          (step $ step initialState)
          (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 2), (0, 3), (0, 4)]}),
      TestCase $ do
        assertEqual
          "Over the edge"
          (step (initialState {snakePosition = [(0, 27), (0, 28), (0, 29)]}))
          (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 28), (0, 29), (0, 0)]}),
      TestCase $ do
        assertEqual
          "Non straight snake"
          (step (initialState {snakePosition = [(0, 0), (0, 1), (1, 1), (1, 2)]}))
          (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 1), (1, 1), (1, 2), (1, 3)]})
    ]

-- Group all tests together
tests :: Test
tests =
  TestList
    [ TestLabel "Initial state" testInitialState,
      TestLabel "Step" testStep
      -- Add more tests here
    ]

-- Main function to run tests
main :: IO ()
main = do
  counts' <- runTestTT tests
  if errors counts' + failures counts' > 0
    then exitFailure
    else exitSuccess
