{-# LANGUAGE RecordWildCards #-}

module GameIO
    ( runGame
    ) where

import Data.List
import Draw (drawRoad, drawSettings)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap
import MoveCars (moveCars)
import RemoveCars (removeCars)
import RenderCars (renderCars)
import System.IO
import System.Random
import Types

display :: Display
display = InWindow "cars" (500, 500) (-500, -500)

bgColor :: Color
bgColor = white

drawApp :: Experiment -> IO Picture
drawApp Experiment{..}
  | (appState == Settings) = return $ drawSettings
  | otherwise = return $ drawRoad road

handleEvent :: Event -> Experiment -> IO Experiment
handleEvent _ state = return state

updateApp :: Float -> Experiment -> IO Experiment
updateApp _ experiment =
  return  $ removeCars $ renderCars $ moveCars experiment

fps :: Int
fps = 30

runGame :: IO ()
runGame = do
  gen <- getStdGen
  let
    roadLane1 = RoadLane 1 [100] []
    roadLane2 = RoadLane 0 [900] []
    roadLane3 = RoadLane (-1) [200] []
    initRoad = Road 5 [roadLane1, roadLane2, roadLane3]
    initState = Experiment gen Motorway (10, 40) (50, 100) [0,0,0] 10 5 initRoad
  playIO display bgColor fps initState drawApp handleEvent updateApp
