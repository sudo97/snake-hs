module Game.UI where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.Functor (void)
import Game.Core
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)

data CustomEvent = Tick

app :: App Snake CustomEvent ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const $ attrMap V.defAttr []
    }

-- | UI rendering function
drawUI :: Snake -> [Widget ()]
drawUI st = [center . border . str $ renderLevel st]

handleEvent :: BrickEvent () CustomEvent -> EventM () Snake ()
handleEvent (AppEvent Tick) = do
  st <- get
  st' <- liftIO $ step' st
  put st'
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
  halt
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  st <- get
  st' <- liftIO . step' $ changeDirection GoUp st
  put st'
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
  st <- get
  st' <- liftIO . step' $ changeDirection GoDown st
  put st'
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = do
  st <- get
  st' <- liftIO . step' $ changeDirection GoLeft st
  put st'
handleEvent (VtyEvent (V.EvKey V.KRight [])) = do
  st <- get
  st' <- liftIO . step' $ changeDirection GoRight st
  put st'
handleEvent _ = pure ()

runGame :: IO ()
runGame = do
  eventChan <- newBChan 10

  _ <- forkIO $ forever $ do
    writeBChan eventChan Tick
    threadDelay 100000 -- 0.1 second delay
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty

  void $ customMain initialVty buildVty (Just eventChan) app initialState'

renderLevel :: Snake -> String
renderLevel (Snake {screenSize = (width, height), snakePosition = position, snackPosition = snack}) =
  unlines $ reverse [renderLine y | y <- [0 .. height - 1]]
  where
    renderLine y = [renderCell (x, y) | x <- [0 .. width - 1]]
    renderCell pt
      | pt `elem` position = 'x'
      | pt == snack = 'o'
      | otherwise = ' '

initialState' :: Snake
initialState' = initialState {snakePosition = [(0, x) | x <- [0 .. 2]], screenSize = (60, 30)}

step' :: Snake -> IO Snake
step' = step initialState'