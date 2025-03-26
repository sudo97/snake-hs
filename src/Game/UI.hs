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

-- | Custom events
data CustomEvent = Tick

-- | App definition
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
drawUI st = [ui]
  where
    ui =
      center $
        border $
          str $
            renderLevel st

-- | Event handler
handleEvent :: BrickEvent () CustomEvent -> EventM () Snake ()
handleEvent (AppEvent Tick) = do
  st <- get
  st' <- liftIO $ step st
  put st'
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  -- Exit on Esc key
  halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
  -- Exit on q key
  halt
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  -- Record Up arrow press
  st <- get
  st' <- liftIO $ step $ changeDirection GoUp st
  put st'
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
  -- Record Down arrow press
  st <- get
  st' <- liftIO $ step $ changeDirection GoDown st
  put st'
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = do
  -- Record Left arrow press
  st <- get
  st' <- liftIO $ step $ changeDirection GoLeft st
  put st'
handleEvent (VtyEvent (V.EvKey V.KRight [])) = do
  -- Record Right arrow press
  st <- get
  st' <- liftIO $ step $ changeDirection GoRight st
  put st'
handleEvent _ = do
  -- Ignore other events
  pure ()

-- | Main function
runGame :: IO ()
runGame = do
  -- Create event channel
  eventChan <- newBChan 10

  -- Fork thread to generate tick events every second
  _ <- forkIO $ forever $ do
    writeBChan eventChan Tick
    threadDelay 100000 -- 0.1 second delay

  -- Initialize Vty
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty

  -- Run the Brick app
  void $ customMain initialVty buildVty (Just eventChan) app (initialState {snakePosition = [(0, x) | x <- [0 .. 20]], screenSize = (60, 30)})

renderLevel :: Snake -> String
renderLevel (Snake {screenSize = (width, height), snakePosition = position, snackPosition = snack}) =
  unlines $ reverse [renderLine y | y <- [0 .. height - 1]]
  where
    renderLine y = [renderCell (x, y) | x <- [0 .. width - 1]]
    renderCell pt
      | pt `elem` position = 'x'
      | pt == snack = 'o'
      | otherwise = ' '