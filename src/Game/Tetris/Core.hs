module Game.Tetris.Core where

import qualified Data.Set as Set

data TetrisGame = TetrisGame
  { screenWidth :: Int,
    screenHeight :: Int,
    figure :: Set.Set Point
  }
  deriving (Show, Eq)

type Point = (Int, Int)

step :: TetrisGame -> TetrisGame
step game = game {figure = if shouldMoveDown then Set.map (\(x, y) -> (x, max (y - 1) 0)) (figure game) else figure game}
  where
    shouldMoveDown = minimum (Set.map snd (figure game)) > 0

rotate :: Set.Set Point -> Set.Set Point
rotate pts =
  let (rotX, rotY) = topLeft pts
      newFigure = Set.map (\(x, y) -> (rotX + y - rotY, rotY - x + rotX)) pts
      (topX, topY) = topLeft newFigure
      deltaX = rotX - topX
      deltaY = rotY - topY
   in Set.map (\(x, y) -> (x + deltaX, y + deltaY)) newFigure
  where
    topLeft = (,) <$> (minimum . Set.map fst) <*> (maximum . Set.map snd)