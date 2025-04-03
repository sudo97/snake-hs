{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TestTetris (testTetris) where

import qualified Data.Set as Set
import Game.Tetris.Core (rotate, step)
import Test.HUnit

testTetris :: Test
testTetris = TestLabel "Tetris" (TestList [testStep, testRotate])

testRotate :: Test
testRotate =
  TestLabel "rotate" $
    TestList
      [ TestCase $ do
          let dot = Set.fromList [(5, 3)]
          let expected = Set.fromList [(5, 3)]
          assertEqual "Dot should rotate" expected (rotate dot)
          let twoPts = Set.fromList [(5, 3), (6, 3)]
          let expected = Set.fromList [(5, 3), (5, 2)]
          assertEqual "Two points should rotate" expected (rotate twoPts)
          let straight = Set.fromList [(5, 3), (5, 2), (5, 1)]
          let expected = Set.fromList [(5, 3), (6, 3), (7, 3)]
          assertEqual "Straight should rotate" expected (rotate straight)
          assertEqual "Straight should rotate 4 times" straight (rotate (rotate (rotate (rotate straight))))
      ]

testStep :: Test
testStep = TestLabel "step" $
  TestCase $ do
    let squareShape = Set.fromList [(0, 10), (0, 9), (1, 10), (1, 9)]
    let expected = Set.fromList [(0, 9), (0, 8), (1, 9), (1, 8)]
    assertEqual "Square shape should move down" expected (step squareShape)
