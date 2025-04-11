{-# LANGUAGE NamedFieldPuns #-}

module Game.Tetris.UI where

import Game.Tetris.Core

ui :: TetrisGame -> String
ui TetrisGame {screenWidth, screenHeight} =
  unlines . replicate screenHeight $ replicate screenWidth ' '
