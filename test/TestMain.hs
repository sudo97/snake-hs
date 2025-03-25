module Main where

-- Import modules from your snake game to test
import Game.Core
import Game.UI (renderLevel)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

testStep :: Test
testStep =
  TestList
    [ TestLabel "Going up" $
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
          ],
      TestLabel "Going down" $
        TestList
          [ TestCase $ do
              pure ()
              -- fail "Implement me"
          ]
    ]

testRenderLevel :: Test
testRenderLevel =
  TestList
    [ TestCase $ do
        let expected =
              unlines
                [ "x"
                ]
        let actual = renderLevel $ initialState {screenSize = (1, 1), snakePosition = [(0, 0)]}
        assertEqual "Render one cell level" expected actual,
      TestCase $ do
        let expected =
              unlines
                [ " ",
                  "x"
                ]
        let actual = renderLevel $ initialState {screenSize = (1, 2), snakePosition = [(0, 0)]}
        assertEqual "Render two cells level" expected actual,
      TestCase $ do
        let expected =
              unlines
                [ " ",
                  "x",
                  " "
                ]
        let actual = renderLevel $ initialState {screenSize = (1, 3), snakePosition = [(0, 1)]}
        assertEqual "Render three cells level" expected actual,
      TestCase $ do
        let expected =
              unlines
                [ " ",
                  "x",
                  "x",
                  "x",
                  " "
                ]
        let actual = renderLevel $ initialState {screenSize = (1, 5), snakePosition = [(0, 1), (0, 2), (0, 3)]}
        assertEqual "Render five cells level" expected actual,
      TestCase $ do
        let expected =
              unlines
                [ "  ",
                  "x ",
                  "x ",
                  "x ",
                  "  "
                ]
        let actual = renderLevel $ initialState {screenSize = (2, 5), snakePosition = [(0, 1), (0, 2), (0, 3)]}
        assertEqual "Render five by two cells level" expected actual,
      TestCase $ do
        let expected =
              unlines
                [ "   ",
                  " x ",
                  " x ",
                  " x ",
                  "   "
                ]
        let actual = renderLevel $ initialState {screenSize = (3, 5), snakePosition = [(1, 1), (1, 2), (1, 3)]}
        assertEqual "Render five by three cells level" expected actual
    ]

-- Group all tests together
tests :: Test
tests =
  TestList
    [ TestLabel "Step" testStep,
      TestLabel "Render Level" testRenderLevel
      -- Add more tests here
    ]

-- Main function to run tests
main :: IO ()
main = do
  counts' <- runTestTT tests
  if errors counts' + failures counts' > 0
    then exitFailure
    else exitSuccess
