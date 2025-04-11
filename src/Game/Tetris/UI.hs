{-# LANGUAGE NamedFieldPuns #-}

module Game.Tetris.UI where

import qualified Data.Set as Set
import Game.Tetris.Core

ui :: TetrisGame -> String
ui TetrisGame {screenWidth, screenHeight, figure} =
  unlines . reverse $ [renderLine y | y <- [0 .. screenHeight - 1]]
  where
    renderPoint (x, y) = if (x, y) `Set.member` figure then 'x' else ' '
    renderLine y = [renderPoint (x, y) | x <- [0 .. screenWidth - 1]]
