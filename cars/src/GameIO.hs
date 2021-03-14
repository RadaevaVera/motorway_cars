{-# LANGUAGE RecordWildCards #-}

module GameIO
    ( runGame
    ) where

import Data.List
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap

import System.IO
import System.Random
import AppState
import Experiment
import Road
import RoadLane
import Car

display :: Display
display = FullScreen --InWindow "cars" (500, 500) (-500, -500)

bgColor :: Color
bgColor = white

drawApp :: AppState -> IO Picture
drawApp (AppState page Experiment{..})
  | (page == Settings) = return $ drawSettings
  | otherwise = return $ drawRoad road

handleEvent :: Event -> AppState -> IO AppState
handleEvent _ state = return state

updateApp :: Float -> AppState -> IO AppState
updateApp _ AppState {..} = return $
  AppState
    page
    (oneStepExperiment experiment)
    --(removeCars $ renderCars $ moveCars experiment)

fps :: Int
fps = 60

runGame :: IO ()
runGame = do
  gen <- getStdGen
  let
    roadLane1 = RoadLane 1 [100] []
    roadLane2 = RoadLane 0 [900] []
    roadLane3 = RoadLane (-1) [200] []
    roadLane4 = RoadLane (-2) [200] []
    initRoad = Road gen 200 [0,0,0,0] [roadLane1, roadLane2, roadLane3, roadLane4]
    initExperiment = Experiment (2, 10) (50, 100) 10 5 initRoad
    initState = AppState Motorway initExperiment
  playIO display bgColor fps initState drawApp handleEvent updateApp

 -- скорость - разделить на 4
 -- частота - умножить на 30