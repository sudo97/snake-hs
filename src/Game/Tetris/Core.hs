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

newFigure :: TetrisGame -> IO (Set.Set Point)
newFigure TetrisGame {screenHeight, screenWidth} = do
  let figures' = figures screenHeight screenWidth
  (figures' !!) <$> randomRIO (0, length figures' - 1)

step :: TetrisGame -> IO TetrisGame
step game@TetrisGame {figure, ground} = do
  let shouldMoveDown = minimum (Set.map snd figure) > 0 && hasNothingUnderFigure game
  cleanUpBottomRow
    <$> if shouldMoveDown
      then pure (game {figure = Set.map (\(x, y) -> (x, max (y - 1) 0)) figure})
      else do
        newRandomFigure <- newFigure game

        pure game {figure = newRandomFigure, ground = Set.union figure ground}

hasNothingUnderFigure :: TetrisGame -> Bool
hasNothingUnderFigure (TetrisGame {figure, ground}) =
  let figurePoints = Set.map (\(x, y) -> (x, y - 1)) figure
   in Set.null (Set.intersection figurePoints ground)

-- TODO: Cleanup not the bottom row, but every row that is full
cleanUpBottomRow :: TetrisGame -> TetrisGame
cleanUpBottomRow g@(TetrisGame {screenWidth, ground}) =
  let canClean = all (\x -> Set.member (x, 0) ground) [0 .. screenWidth - 1]
   in if canClean
        then cleanUpBottomRow (g {ground = Set.map (\(x, y) -> (x, y - 1)) (Set.filter (\(_, y) -> y > 0) ground)})
        else g

rotate :: Set.Set Point -> Set.Set Point
rotate pts =
  let (rotX, rotY) = topLeft pts
      rotatedFigure = Set.map (\(x, y) -> (rotX + y - rotY, rotY - x + rotX)) pts
      (topX, topY) = topLeft rotatedFigure
      deltaX = rotX - topX
      deltaY = rotY - topY
   in Set.map (\(x, y) -> (x + deltaX, y + deltaY)) rotatedFigure
  where
    topLeft = (,) <$> minimum . Set.map fst <*> maximum . Set.map snd

-- TODO: add collision with ground set
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
