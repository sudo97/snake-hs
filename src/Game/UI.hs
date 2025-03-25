module Game.UI where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Functor (void)
import Data.List (transpose)
import Game.Core
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)

-- | Custom events
data CustomEvent = Tick

-- | App definition
app :: App St CustomEvent ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const $ attrMap V.defAttr []
    }

-- | UI rendering function
drawUI :: St -> [Widget ()]
drawUI st = [ui]
  where
    ui =
      center $
        border $
          vBox
            [ padAll 1 $ str $ "Counter: " ++ show (stCounter st),
              padAll 1 $ str $ "Direction: " ++ maybe "None" show (stDirection st),
              padAll 1 $ str "Press Esc or q to exit"
            ]

-- | Event handler
handleEvent :: BrickEvent () CustomEvent -> EventM () St ()
handleEvent (AppEvent Tick) = do
  st <- get
  put $ st {stCounter = stCounter st + 1}
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  -- Exit on Esc key
  halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
  -- Exit on q key
  halt
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  -- Record Up arrow press
  st <- get
  put $ st {stDirection = Just GoUp}
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
  -- Record Down arrow press
  st <- get
  put $ st {stDirection = Just GoDown}
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = do
  -- Record Left arrow press
  st <- get
  put $ st {stDirection = Just GoLeft}
handleEvent (VtyEvent (V.EvKey V.KRight [])) = do
  -- Record Right arrow press
  st <- get
  put $ st {stDirection = Just GoRight}
handleEvent _ = do
  -- Ignore other events
  pure ()

-- | Initial app state
initialState :: St
initialState = St {stCounter = 0, stDirection = Nothing}

-- | Main function
runGame :: IO ()
runGame = do
  -- Create event channel
  eventChan <- newBChan 10

  -- Fork thread to generate tick events every second
  _ <- forkIO $ forever $ do
    writeBChan eventChan Tick
    threadDelay 1000000 -- 1 second delay

  -- Initialize Vty
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty

  -- Run the Brick app
  void $ customMain initialVty buildVty (Just eventChan) app Game.UI.initialState

renderLevel :: Snake -> String
renderLevel (Snake {screenSize = (width, height), snakePosition = position}) =
  unlines $ reverse [renderLine y | y <- [0 .. height - 1]]
  where
    renderLine y = [renderCell (x, y) | x <- [0 .. width - 1]]
    renderCell (x, y) = if (x, y) `elem` position then 'x' else ' '