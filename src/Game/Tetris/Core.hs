{-# LANGUAGE NamedFieldPuns #-}

module Game.Tetris.Core where

import qualified Data.Set as Set
import System.Random (randomRIO)

data TetrisGame = TetrisGame
  { screenWidth :: Int,
    screenHeight :: Int,
    figure :: Set.Set Point,
    ground :: Set.Set Point
  }
  deriving (Show, Eq)

type Point = (Int, Int)

figures :: Int -> Int -> [Set.Set Point]
figures height width =
  let topY = height - 1
      midX = width `div` 2
   in [ Set.fromList [(midX - 2, topY), (midX - 1, topY), (midX, topY), (midX + 1, topY)],
        Set.fromList [(midX - 1, topY), (midX, topY), (midX + 1, topY), (midX, topY - 1)],
        Set.fromList [(midX - 1, topY), (midX, topY), (midX + 1, topY - 1), (midX, topY - 1)],
        Set.fromList [(midX + 1, topY), (midX, topY), (midX + 1, topY - 1), (midX, topY - 1)],
        Set.fromList [(midX + 1, topY), (midX, topY), (midX - 1, topY - 1), (midX, topY - 1)]
      ]

step :: TetrisGame -> IO TetrisGame
step game = do
  let shouldMoveDown = minimum (Set.map snd (figure game)) > 0
  if shouldMoveDown
    then do
      pure game {figure = Set.map (\(x, y) -> (x, max (y - 1) 0)) (figure game)}
    else do
      let figures' = figures (screenHeight game) (screenWidth game)
      newIdx <- randomRIO (0, length figures' - 1)
      let newFigure = figures' !! newIdx
      pure game {figure = newFigure, ground = Set.union (figure game) (ground game)}

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

moveLeft :: TetrisGame -> TetrisGame
moveLeft game@(TetrisGame {figure}) =
  let shouldMoveLeft = minimum (Set.map fst figure) > 0
      figure' = if shouldMoveLeft then Set.map (\(x, y) -> (x - 1, y)) figure else figure
   in game {figure = figure'}

moveRight :: TetrisGame -> TetrisGame
moveRight game@(TetrisGame {figure, screenWidth}) =
  let shouldMoveRight = maximum (Set.map fst figure) < screenWidth - 1
      figure' = if shouldMoveRight then Set.map (\(x, y) -> (x + 1, y)) figure else figure
   in game {figure = figure'}
