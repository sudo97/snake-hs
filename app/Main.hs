module Main where

import Game
import Game.Tetris.UI (runTetrisGame)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runSnakeGame
    ["snake"] -> runSnakeGame
    ["tetris"] -> runTetrisGame
    _ -> putStrLn "Invalid game"
