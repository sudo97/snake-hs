module Game.Core where

import System.Random (randomRIO)

data SnakeDirection = GoUp | GoDown | GoLeft | GoRight deriving (Show, Eq)

data Snake = Snake
  { screenSize :: (Int, Int),
    snakeDirection :: SnakeDirection,
    snakePosition :: [(Int, Int)],
    snackPosition :: (Int, Int)
  }
  deriving (Show, Eq)

initialState :: Snake
initialState =
  Snake
    { screenSize = (30, 30),
      snakeDirection = GoUp,
      snakePosition = [(0, 0), (0, 1), (0, 2)],
      snackPosition = (15, 15)
    }

step :: Snake -> Snake -> IO Snake
step fallBackState s@(Snake {snakeDirection = dir, snakePosition = position, screenSize = (width, height), snackPosition = snack}) =
  do
    newSnack <- (,) <$> randomRIO (0, width - 1) <*> randomRIO (0, height - 1)
    let tail' = drop 1 position
        (x, y) = last position
        newLastItem = case dir of
          GoUp -> (x, if y + 1 >= height then 0 else y + 1)
          GoDown -> (x, if y - 1 < 0 then height - 1 else y - 1)
          GoLeft -> (if x - 1 < 0 then width - 1 else x - 1, y)
          GoRight -> (if x + 1 >= width then 0 else x + 1, y)
    let snackPos = if newLastItem == snack then newSnack else snack
    let snakePos = if newLastItem == snack then position ++ [newLastItem] else tail' ++ [newLastItem]
    pure $
      if hasCollisions snakePos
        then fallBackState
        else
          s {snakePosition = snakePos, snackPosition = snackPos}

hasCollisions :: [(Int, Int)] -> Bool
hasCollisions [] = False
hasCollisions (x : xs) = x `elem` xs || hasCollisions xs

changeDirection :: SnakeDirection -> Snake -> Snake
changeDirection dir s@(Snake {snakeDirection = currentDir}) = s {snakeDirection = if dir == oppositeDir currentDir then currentDir else dir}
  where
    oppositeDir GoUp = GoDown
    oppositeDir GoDown = GoUp
    oppositeDir GoLeft = GoRight
    oppositeDir GoRight = GoLeft
