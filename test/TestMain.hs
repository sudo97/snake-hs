module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import TestSnake (testSnake)

tests :: Test
tests =
  TestList
    [ testSnake
    ]

-- Main function to run tests
main :: IO ()
main = do
  counts' <- runTestTT tests
  if errors counts' + failures counts' > 0
    then exitFailure
    else exitSuccess
