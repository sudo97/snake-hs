{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TestTetris (testTetris) where

import qualified Data.Set as Set
import Game.Tetris.Core (TetrisGame (..), rotate, step)
import Game.Tetris.UI (ui)
import Test.HUnit

testTetris :: Test
testTetris = TestLabel "Tetris" (TestList [testStep, testRotate, testUI])

testRotate :: Test
testRotate =
  TestLabel "rotate" $
    TestList
      [ TestCase $ do
          let dot = Set.fromList [(5, 3)]
          let expected = Set.fromList [(5, 3)]
          assertEqual
            "Dot should not change when rotated"
            expected
            (rotate dot),
        TestCase $ do
          let twoPts = Set.fromList [(5, 3), (6, 3)]
          let expected = Set.fromList [(5, 3), (5, 2)]
          assertEqual
            "Two points should rotate around the top left point"
            expected
            (rotate twoPts),
        TestCase $ do
          let straight = Set.fromList [(5, 3), (5, 2), (5, 1)]
          let expected = Set.fromList [(5, 3), (6, 3), (7, 3)]
          assertEqual
            "Straight line should rotate around the top left point"
            expected
            (rotate straight),
        TestCase $ do
          let straight = Set.fromList [(5, 3), (5, 2), (5, 1)]
          assertEqual
            "Straight line should rotate 4 times around the top left point and return to the original position"
            straight
            (rotate (rotate (rotate (rotate straight))))
      ]

testStep :: Test
testStep =
  TestLabel "step" $
    TestList
      [ TestCase $ do
          let squareShape = Set.fromList [(0, 10), (0, 9), (1, 10), (1, 9)]
          let expected = Set.fromList [(0, 9), (0, 8), (1, 9), (1, 8)]
          assertEqual "Square shape should move down" expected (step squareShape),
        TestCase $ do
          let nearTheBottom = Set.fromList [(0, 0)]
          let expected = Set.fromList [(0, 0)]
          assertEqual "Shape should not move down when near the bottom" expected (step nearTheBottom)
      ]

testUI :: Test
testUI =
  TestLabel "UI" $
    TestList
      [ TestCase $ do
          let game = TetrisGame {screenWidth = 3, screenHeight = 3, figure = Set.fromList [(0, 2), (1, 2), (2, 2)]}
          let expected =
                unlines
                  [ "xxx",
                    "   ",
                    "   "
                  ]
          assertEqual "UI should return a 3x3 grid of spaces" expected (ui game)
      ]