module Main where

import Game
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runSnakeGame
    ["snake"] -> runSnakeGame
    _ -> putStrLn "Invalid game"
