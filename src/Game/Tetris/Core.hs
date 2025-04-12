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
        Set.fromList [(midX + 1, topY), (midX, topY), (midX - 1, topY - 1), (midX, topY - 1)],
        Set.fromList [(midX, topY), (midX + 1, topY), (midX + 2, topY), (midX + 2, topY - 1)],
        Set.fromList [(midX, topY), (midX + 1, topY), (midX + 2, topY), (midX, topY - 1)]
      ]

newFigure :: TetrisGame -> IO (Set.Set Point)
newFigure TetrisGame {screenHeight, screenWidth} = do
  let figures' = figures screenHeight screenWidth
  (figures' !!) <$> randomRIO (0, length figures' - 1)

step :: TetrisGame -> IO TetrisGame
step game@TetrisGame {figure, ground} = do
  let shouldMoveDown = minimum (Set.map snd figure) > 0 && hasNothingUnderFigure game
  newGame <-
    cleanUpCompletedRows
      <$> if shouldMoveDown
        then pure (game {figure = Set.map (\(x, y) -> (x, max (y - 1) 0)) figure})
        else do
          newRandomFigure <- newFigure game
          pure game {figure = newRandomFigure, ground = Set.union figure ground}
  pure $ if isGameOver newGame then newGame {ground = Set.empty} else newGame

isGameOver :: TetrisGame -> Bool
isGameOver (TetrisGame {screenHeight, ground}) =
  let topY = maximum (Set.map snd ground)
   in not (Set.null ground) && topY >= screenHeight - 1

hasNothingUnderFigure :: TetrisGame -> Bool
hasNothingUnderFigure (TetrisGame {figure, ground}) =
  let figurePoints = Set.map (\(x, y) -> (x, y - 1)) figure
   in Set.null (Set.intersection figurePoints ground)

cleanUpCompletedRows :: TetrisGame -> TetrisGame
cleanUpCompletedRows game@(TetrisGame {screenWidth, ground, screenHeight}) =
  let ys = [0 .. screenHeight - 1]
   in case filter (\y -> all (\x -> Set.member (x, y) ground) [0 .. screenWidth - 1]) ys of
        [] -> game
        (y : _) ->
          cleanUpCompletedRows
            ( game
                { ground = Set.map (\(x, y') -> (x, if y' > y then y' - 1 else y')) (Set.filter (\(_, y') -> y' /= y) ground)
                }
            )

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

moveLeft :: TetrisGame -> TetrisGame
moveLeft = moveFigure (-1)

moveRight :: TetrisGame -> TetrisGame
moveRight = moveFigure 1

moveFigure :: Int -> TetrisGame -> TetrisGame
moveFigure delta game@(TetrisGame {figure, ground, screenWidth}) =
  let figure' = Set.map (\(x, y) -> (x + delta, y)) figure
   in game {figure = if isSafeToMove figure' then figure' else figure}
  where
    isSafeToMove figure' = minimum (Set.map fst figure') >= 0 && maximum (Set.map fst figure') <= screenWidth - 1 && Set.null (Set.intersection figure' ground)
