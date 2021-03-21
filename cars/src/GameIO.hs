module GameIO
    ( runGame
    ) where

import AppState
import Graphics.Gloss.Interface.IO.Game
import System.IO
import System.Random

display :: Display
display = FullScreen --InWindow "cars" (500, 500) (-500, -500)

bgColor :: Color
bgColor = white

drawApp :: AppState -> IO Picture
drawApp appState = return $ drawAppState appState

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) appState
  = eventAppState appState x y
handleEvent _ appState = return appState

updateApp :: Float -> AppState -> IO AppState
updateApp _ app = return $ updateAppState app

fps :: Int
fps = 60

runGame :: IO ()
runGame = do
  gen <- getStdGen
  playIO display bgColor fps (initState gen) drawApp handleEvent updateApp
