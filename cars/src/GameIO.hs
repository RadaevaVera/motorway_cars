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
  | (page == Settings) = return $ drawSettings rangeV rangeT deltaV deltaT
  | otherwise = return $ drawRoad road

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) (AppState Settings Experiment{..})
  = return $ AppState Settings Experiment{..}
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) (AppState Motorway exp)
    | (10 + 1 * 150) < y && y < (150 + 1 * 150) = return $ AppState Motorway (addSlowdown exp 1 (x + 1000))
    | (10 + 0 * 150) < y && y < (150 + 0 * 150) = return $ AppState Motorway (addSlowdown exp 0 (x + 1000))
    | (10 - 1 * 150) < y && y < (150 - 1 * 150) = return $ AppState Motorway (addSlowdown exp (-1) (x + 1000))
    | (10 - 2 * 150) < y && y < (150 - 2 * 150) = return $ AppState Motorway (addSlowdown exp (-2) (x + 1000))
    | (10 - 4 * 150) < y && y < (150 - 3.5 * 150) = return $ AppState Settings exp
    | otherwise = return $ AppState Motorway exp
handleEvent _ appState = return appState

updateApp :: Float -> AppState -> IO AppState
updateApp _ AppState{..}
  | page == Motorway = return $ AppState page (oneStepExperiment experiment)
  | otherwise = return $ AppState{..}

fps :: Int
fps = 60

runGame :: IO ()
runGame = do
  gen <- getStdGen
  let
    roadLane1 = RoadLane 1 [] []
    roadLane2 = RoadLane 0 [] []
    roadLane3 = RoadLane (-1) [] []
    roadLane4 = RoadLane (-2) [] []
    initRoad = Road gen 200 [0,0,0,0] [roadLane1, roadLane2, roadLane3, roadLane4]
    initExperiment = Experiment (5, 15) (50, 100) 5 200 initRoad
    initState = AppState Settings initExperiment
  playIO display bgColor fps initState drawApp handleEvent updateApp

 -- скорость - разделить на 4
 -- частота - умножить на 30