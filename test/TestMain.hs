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
                (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 1), (0, 2), (0, 3)]})
                (step initialState),
            TestCase $ do
              assertEqual
                "Step twice"
                (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 2), (0, 3), (0, 4)]})
                (step $ step initialState),
            TestCase $ do
              assertEqual
                "Over the edge"
                (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 28), (0, 29), (0, 0)]})
                (step (initialState {snakePosition = [(0, 27), (0, 28), (0, 29)]})),
            TestCase $ do
              assertEqual
                "Non straight snake"
                (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 1), (1, 1), (1, 2), (1, 3)]})
                (step (initialState {snakePosition = [(0, 0), (0, 1), (1, 1), (1, 2)]}))
          ],
      TestLabel "Going down" $
        TestList
          [ TestCase $ do
              assertEqual
                "Step"
                (Snake {screenSize = (30, 30), snakeDirection = GoDown, snakePosition = [(0, 1), (0, 0), (0, 29)]})
                (step initialState {snakePosition = [(0, 2), (0, 1), (0, 0)], snakeDirection = GoDown}),
            TestCase $ do
              assertEqual
                "Step twice"
                (Snake {screenSize = (30, 30), snakeDirection = GoDown, snakePosition = [(0, 0), (0, 29), (0, 28)]})
                (step $ step initialState {snakePosition = [(0, 2), (0, 1), (0, 0)], snakeDirection = GoDown}),
            TestCase $ do
              assertEqual
                "Non straight snake"
                (Snake {screenSize = (30, 30), snakeDirection = GoDown, snakePosition = [(1, 1), (0, 1), (0, 0), (0, 29)]})
                (step (initialState {snakePosition = [(1, 2), (1, 1), (0, 1), (0, 0)], snakeDirection = GoDown}))
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
