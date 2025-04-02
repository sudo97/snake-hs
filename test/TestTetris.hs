module TestTetris (testTetris) where

import Test.HUnit

testTetris :: Test
testTetris =
  TestLabel
    "Tetris"
    ( TestList
        [ TestCase $ do
            assertFailure "Not implemented"
        ]
    )
