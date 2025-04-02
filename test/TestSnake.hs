module TestSnake (testSnake) where

import Game.Snake.Core
  ( Snake (..),
    SnakeDirection (..),
    changeDirection,
    initialState,
    step,
  )
import Game.Snake.UI (renderLevel)
import Test.HUnit

testSnake :: Test
testSnake =
  TestLabel
    "Snake"
    ( TestList
        [ TestLabel "Step" testStep,
          TestLabel "Render Level" testRenderLevel,
          TestLabel "Change Direction" testChangeDirection
          -- Add more tests here
        ]
    )

testChangeDirection :: Test
testChangeDirection =
  TestList
    [ TestCase $ do
        assertEqual "Change direction Up -> Up is allowed" (initialState {snakeDirection = GoUp}) (changeDirection GoUp initialState)
        assertEqual "Change direction Up -> Down not allowed" (initialState {snakeDirection = GoUp}) (changeDirection GoDown initialState)
        assertEqual "Change direction Up -> Left is allowed" (initialState {snakeDirection = GoLeft}) (changeDirection GoLeft initialState)
        assertEqual "Change direction Up -> Right is allowed" (initialState {snakeDirection = GoRight}) (changeDirection GoRight initialState),
      TestCase $ do
        assertEqual "Change direction Down -> Up is not allowed" (initialState {snakeDirection = GoDown}) (changeDirection GoUp (initialState {snakeDirection = GoDown}))
        assertEqual "Change direction Down -> Down is allowed" (initialState {snakeDirection = GoDown}) (changeDirection GoDown (initialState {snakeDirection = GoDown}))
        assertEqual "Change direction Down -> Left is allowed" (initialState {snakeDirection = GoLeft}) (changeDirection GoLeft (initialState {snakeDirection = GoDown}))
        assertEqual "Change direction Down -> Right is allowed" (initialState {snakeDirection = GoRight}) (changeDirection GoRight (initialState {snakeDirection = GoDown})),
      TestCase $ do
        assertEqual "Change direction Left -> Up is allowed" (initialState {snakeDirection = GoUp}) (changeDirection GoUp (initialState {snakeDirection = GoLeft}))
        assertEqual "Change direction Left -> Down is allowed" (initialState {snakeDirection = GoDown}) (changeDirection GoDown (initialState {snakeDirection = GoLeft}))
        assertEqual "Change direction Left -> Left is allowed" (initialState {snakeDirection = GoLeft}) (changeDirection GoLeft (initialState {snakeDirection = GoLeft}))
        assertEqual "Change direction Left -> Right is not allowed" (initialState {snakeDirection = GoLeft}) (changeDirection GoRight (initialState {snakeDirection = GoLeft})),
      TestCase $ do
        assertEqual "Change direction Right -> Up is allowed" (initialState {snakeDirection = GoUp}) (changeDirection GoUp (initialState {snakeDirection = GoRight}))
        assertEqual "Change direction Right -> Down is allowed" (initialState {snakeDirection = GoDown}) (changeDirection GoDown (initialState {snakeDirection = GoRight}))
        assertEqual "Change direction Right -> Left is not allowed" (initialState {snakeDirection = GoRight}) (changeDirection GoLeft (initialState {snakeDirection = GoRight}))
        assertEqual "Change direction Right -> Right is allowed" (initialState {snakeDirection = GoRight}) (changeDirection GoRight (initialState {snakeDirection = GoRight}))
    ]

testStep :: Test
testStep =
  TestList
    [ TestLabel "Going up" $
        TestList
          [ TestCase $ do
              result <- step undefined initialState
              assertEqual
                "Step"
                (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 1), (0, 2), (0, 3)], snackPosition = (15, 15)})
                result,
            TestCase $ do
              result <- step undefined initialState >>= step undefined
              assertEqual
                "Step twice"
                (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 2), (0, 3), (0, 4)], snackPosition = (15, 15)})
                result,
            TestCase $ do
              result <- step undefined (initialState {snakePosition = [(0, 27), (0, 28), (0, 29)]})
              assertEqual
                "Over the edge"
                (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 28), (0, 29), (0, 0)], snackPosition = (15, 15)})
                result,
            TestCase $ do
              result <- step undefined (initialState {snakePosition = [(0, 0), (0, 1), (1, 1), (1, 2)]})
              assertEqual
                "Non straight snake"
                (Snake {screenSize = (30, 30), snakeDirection = GoUp, snakePosition = [(0, 1), (1, 1), (1, 2), (1, 3)], snackPosition = (15, 15)})
                result
          ],
      TestLabel "Going down" $
        TestList
          [ TestCase $ do
              result <- step undefined (initialState {snakePosition = [(0, 2), (0, 1), (0, 0)], snakeDirection = GoDown})
              assertEqual
                "Step"
                (Snake {screenSize = (30, 30), snakeDirection = GoDown, snakePosition = [(0, 1), (0, 0), (0, 29)], snackPosition = (15, 15)})
                result,
            TestCase $ do
              result <- step undefined (initialState {snakePosition = [(0, 2), (0, 1), (0, 0)], snakeDirection = GoDown}) >>= step undefined
              assertEqual
                "Step twice"
                (Snake {screenSize = (30, 30), snakeDirection = GoDown, snakePosition = [(0, 0), (0, 29), (0, 28)], snackPosition = (15, 15)})
                result,
            TestCase $ do
              result <- step undefined (initialState {snakePosition = [(1, 2), (1, 1), (0, 1), (0, 0)], snakeDirection = GoDown})
              assertEqual
                "Non straight snake"
                (Snake {screenSize = (30, 30), snakeDirection = GoDown, snakePosition = [(1, 1), (0, 1), (0, 0), (0, 29)], snackPosition = (15, 15)})
                result
          ],
      TestLabel "Going left" $
        TestList
          [ TestCase $ do
              result <- step undefined (initialState {snakePosition = [(2, 0), (1, 0), (0, 0)], snakeDirection = GoLeft})
              assertEqual
                "Step"
                (Snake {screenSize = (30, 30), snakeDirection = GoLeft, snakePosition = [(1, 0), (0, 0), (29, 0)], snackPosition = (15, 15)})
                result
          ],
      TestLabel "Going right" $
        TestList
          [ TestCase $ do
              result <- step undefined (initialState {snakePosition = [(0, 0), (1, 0), (1, 1)], snakeDirection = GoRight})
              assertEqual
                "Step"
                (Snake {screenSize = (30, 30), snakeDirection = GoRight, snakePosition = [(1, 0), (1, 1), (2, 1)], snackPosition = (15, 15)})
                result
          ],
      TestLabel "Eating and growing" $
        TestList
          [ TestCase $ do
              result <- step undefined (initialState {snakePosition = [(0, 0), (0, 1), (0, 2)], snackPosition = (0, 3)})
              assertEqual
                "Step"
                [(0, 0), (0, 1), (0, 2), (0, 3)]
                (snakePosition result)
              if snackPosition result == (0, 3)
                then do
                  print $ snackPosition result
                  assertFailure "Snack is eaten"
                else do
                  let (x, y) = snackPosition result
                  assertBool "Snack is within the screen" (x >= 0 && x < 30 && y >= 0 && y < 30)
          ],
      TestLabel "Collisions" $
        TestList
          [ TestCase $ do
              result <- step initialState (initialState {snakePosition = [(0, 0), (0, 1), (0, 2), (1, 2), (1, 1)], snakeDirection = GoLeft})
              assertEqual "Step" initialState result
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
        assertEqual "Render five by three cells level" expected actual,
      TestCase $ do
        let expected =
              unlines
                [ "   ",
                  " o ",
                  "   ",
                  " x ",
                  " x "
                ]
        let actual = renderLevel $ initialState {screenSize = (3, 5), snakePosition = [(1, 0), (1, 1)], snackPosition = (1, 3)}
        assertEqual "Render five by three cells level with snack" expected actual
    ]
