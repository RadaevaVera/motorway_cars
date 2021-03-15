{-# LANGUAGE RecordWildCards #-}

module GameIO
    ( runGame
    ) where

import Data.List
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap
import System.IO
import System.Exit (exitSuccess)
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
  | (10 - 2.5 * 150) < y && y < (150 - 2.5 * 150) = do
    gen <- getStdGen
    let exp = Experiment rangeV rangeT deltaV deltaT (initRoad gen)
    return $ AppState Motorway exp
  | (10 - 3.5 * 150) < y && y < (150 - 3.5 * 150) = exitSuccess
  | (90 + 2 * 150) < y && y < (120 + 2 * 150) && 500 < x && x < 540 = return $ AppState Settings (Experiment (addL 1 rangeV) rangeT deltaV deltaT road)
  | (40 + 2 * 150) < y && y < (70 + 2 * 150) && 500 < x && x < 540 = return $ AppState Settings (Experiment (subL 1 rangeV) rangeT deltaV deltaT road)
  | (90 + 1 * 150) < y && y < (120 + 1 * 150) && 500 < x && x < 540 = return $ AppState Settings (Experiment rangeV (addL 60 rangeT) deltaV deltaT road)
  | (40 + 1 * 150) < y && y < (70 + 1 * 150) && 500 < x && x < 540 = return $ AppState Settings (Experiment rangeV (subL 60 rangeT) deltaV deltaT road)
  | (90 + 2 * 150) < y && y < (120 + 2 * 150) && 710 < x && x < 750 = return $ AppState Settings (Experiment (addR 1 rangeV) rangeT deltaV deltaT road)
  | (40 + 2 * 150) < y && y < (70 + 2 * 150) && 710 < x && x < 750 = return $ AppState Settings (Experiment (subR 1 rangeV) rangeT deltaV deltaT road)
  | (90 + 1 * 150) < y && y < (120 + 1 * 150) && 710 < x && x < 750 = return $ AppState Settings (Experiment rangeV (addR 60 rangeT) deltaV deltaT road)
  | (40 + 1 * 150) < y && y < (70 + 1 * 150) && 710 < x && x < 750 = return $ AppState Settings (Experiment rangeV (subR 60 rangeT) deltaV deltaT road)
  | (90 + 0 * 150) < y && y < (120 + 0 * 150) && 500 < x && x < 540 = return $ AppState Settings (Experiment rangeV rangeT (deltaV + 1) deltaT road)
  | (40 + 0 * 150) < y && y < (70 + 0 * 150) && 500 < x && x < 540 = return $ AppState Settings (Experiment rangeV rangeT (deltaV - 1) deltaT road)
  | (90 - 1 * 150) < y && y < (120 - 1 * 150) && 500 < x && x < 540 = return $ AppState Settings (Experiment rangeV rangeT deltaV (deltaT + 60) road)
  | (40 - 1 * 150) < y && y < (70 - 1 * 150) && 500 < x && x < 540 = return $ AppState Settings (Experiment rangeV rangeT deltaV (deltaT - 60) road)
  | otherwise = return $ AppState Settings Experiment{..}
  where
    addL n (r1, r2) = (r1 + n, r2)
    subL n (r1, r2) = (r1 - n, r2)
    addR n (r1, r2) = (r1, r2 + n)
    subR n (r1, r2) = (r1, r2 - n)
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

initRoad :: StdGen -> Road
initRoad gen =
  let
    roadLane1 = RoadLane 1 [] []
    roadLane2 = RoadLane 0 [] []
    roadLane3 = RoadLane (-1) [] []
    roadLane4 = RoadLane (-2) [] []
  in Road gen 180 [0,0,0,0] [roadLane1, roadLane2, roadLane3, roadLane4]


runGame :: IO ()
runGame = do
  gen <- getStdGen
  let
    initExperiment = Experiment (10, 15) (60, 120) 1 180 (initRoad gen)
    initState = AppState Settings initExperiment
  playIO display bgColor fps initState drawApp handleEvent updateApp
