module Game.Core where

data SnakeDirection = GoUp | GoDown | GoLeft | GoRight deriving (Show, Eq)

data Snake = Snake
  { screenSize :: (Int, Int),
    snakeDirection :: SnakeDirection,
    snakePosition :: [(Int, Int)]
  }
  deriving (Show, Eq)

initialState :: Snake
initialState =
  Snake
    { screenSize = (30, 30),
      snakeDirection = GoUp,
      snakePosition = [(0, 0), (0, 1), (0, 2)]
    }

step :: Snake -> Snake
step s@(Snake {snakeDirection = GoUp, snakePosition = position, screenSize = (_, height)}) =
  s
    { snakeDirection = GoUp,
      snakePosition =
        let tail' = drop 1 position
            (x, y) = last position
            newLastItem = (x, if y + 1 >= height then 0 else y + 1)
         in tail' ++ [newLastItem]
    }
step s = s

-- | App state
data St = St
  { stCounter :: Int,
    stDirection :: Maybe SnakeDirection
  }