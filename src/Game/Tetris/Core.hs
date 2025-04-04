module Game.Tetris.Core (TetrisGame (..), step, rotate) where

import qualified Data.Set as Set

data TetrisGame = TetrisGame {}

type Point = (Int, Int)

step :: Set.Set Point -> Set.Set Point
step = Set.map (\(x, y) -> (x, max (y - 1) 0))

topLeft :: Set.Set Point -> Point
topLeft pts = (minimum (Set.map fst pts), maximum (Set.map snd pts))

rotate :: Set.Set Point -> Set.Set Point
rotate pts =
  let (rotX, rotY) = topLeft pts
      newFigure = Set.map (\(x, y) -> (rotX + y - rotY, rotY - x + rotX)) pts
      (topX, topY) = topLeft newFigure
      deltaX = rotX - topX
      deltaY = rotY - topY
   in Set.map (\(x, y) -> (x + deltaX, y + deltaY)) newFigure