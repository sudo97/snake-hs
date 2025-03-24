module Game.Core where

data SnakeDirection = Up | Down | Left | Right deriving (Show, Eq)

data Snake = Snake {
  screenSize :: (Int, Int),
  snakeDirection :: SnakeDirection,
  snakePosition :: [(Int, Int)]
} deriving (Show, Eq)

initialState :: Snake
initialState = Snake {
  screenSize = (10, 10),
  snakeDirection = Game.Core.Right,
  snakePosition = [(0, 0), (0, 1), (0, 2)]
  }

-- | App state
data St = St
  { stCounter :: Int
  , stDirection :: Maybe SnakeDirection
  }