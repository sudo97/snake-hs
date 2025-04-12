{-# LANGUAGE NamedFieldPuns #-}

module Game.Tetris.UI where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import qualified Data.Set as Set
import Game.Tetris.Core (TetrisGame (..), moveLeft, moveRight, rotate, step)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)

data CustomEvent = Tick

ui :: TetrisGame -> String
ui TetrisGame {screenWidth, screenHeight, figure} =
  unlines . reverse $ [renderLine y | y <- [0 .. screenHeight - 1]]
  where
    renderPoint (x, y) = if (x, y) `Set.member` figure then 'x' else ' '
    renderLine y = [renderPoint (x, y) | x <- [0 .. screenWidth - 1]]

runTetrisGame :: IO ()
runTetrisGame = do
  eventChan <- newBChan 10

  _ <- forkIO $ forever $ do
    writeBChan eventChan Tick
    threadDelay 300000 -- 0.1 second delay
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty

  void $ customMain initialVty buildVty (Just eventChan) app initialState

app :: App TetrisGame CustomEvent ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const $ attrMap V.defAttr []
    }

drawUI :: TetrisGame -> [Widget ()]
drawUI st = [center . border . str $ ui st]

handleEvent :: BrickEvent () CustomEvent -> EventM () TetrisGame ()
handleEvent (AppEvent Tick) = do
  st <- get
  liftIO (step st) >>= put
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
  halt
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  st <- get
  put $ st {figure = rotate (figure st)}
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
  st <- get
  liftIO (step st) >>= put
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = do
  st <- get
  put $ moveLeft st
handleEvent (VtyEvent (V.EvKey V.KRight [])) = do
  st <- get
  put $ moveRight st
handleEvent _ = pure ()

initialState :: TetrisGame
initialState = TetrisGame {screenWidth = 10, screenHeight = 20, figure = Set.fromList [(0, 19), (1, 19), (2, 19), (1, 18)], ground = Set.empty}
