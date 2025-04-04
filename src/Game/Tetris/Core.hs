module Game.Tetris.Core where

import qualified Data.Set as Set

data TetrisGame = TetrisGame {}

type Point = (Int, Int)

step :: Set.Set Point -> Set.Set Point
step = Set.map (\(x, y) -> (x, max (y - 1) 0))

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