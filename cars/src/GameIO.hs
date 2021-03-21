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
drawApp (AppState page exp)
  | (page == Settings) = return $ drawSettings (getSettings exp)
  | otherwise = return $ Pictures [drawButtoms ,drawRoad road, drawAccident road]
  where road = getRoad exp

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) (AppState Settings exp)
  | (10 - 2.5 * 150) < y && y < (150 - 2.5 * 150) = do
    gen <- getStdGen
    return $ AppState Motorway (newExperiment exp gen)
  | (10 - 3.5 * 150) < y && y < (150 - 3.5 * 150) = exitSuccess
  | (90 + 2 * 150) < y && y < (120 + 2 * 150) && 500 < x && x < 540 = return $ AppState Settings (addRangeV_L exp)
  | (40 + 2 * 150) < y && y < (70 + 2 * 150) && 500 < x && x < 540 = return $ AppState Settings (subRangeV_L exp)
  | (90 + 1 * 150) < y && y < (120 + 1 * 150) && 500 < x && x < 540 = return $ AppState Settings (addRangeT_L exp)
  | (40 + 1 * 150) < y && y < (70 + 1 * 150) && 500 < x && x < 540 = return $ AppState Settings (subRangeT_L exp)
  | (90 + 2 * 150) < y && y < (120 + 2 * 150) && 710 < x && x < 750 = return $ AppState Settings (addRangeV_R exp)
  | (40 + 2 * 150) < y && y < (70 + 2 * 150) && 710 < x && x < 750 = return $ AppState Settings (subRangeV_R exp)
  | (90 + 1 * 150) < y && y < (120 + 1 * 150) && 710 < x && x < 750 = return $ AppState Settings (addRangeT_R exp)
  | (40 + 1 * 150) < y && y < (70 + 1 * 150) && 710 < x && x < 750 = return $ AppState Settings (subRangeT_R exp)
  | (90 + 0 * 150) < y && y < (120 + 0 * 150) && 500 < x && x < 540 = return $ AppState Settings (addDeltaV exp)
  | (40 + 0 * 150) < y && y < (70 + 0 * 150) && 500 < x && x < 540 = return $ AppState Settings (subDeltaV exp)
  | (90 - 1 * 150) < y && y < (120 - 1 * 150) && 500 < x && x < 540 = return $ AppState Settings (addDeltaT exp)
  | (40 - 1 * 150) < y && y < (70 - 1 * 150) && 500 < x && x < 540 = return $ AppState Settings (subDeltaT exp)
  | otherwise = return $ AppState Settings exp
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) (AppState Motorway exp)
  | (10 + 1 * 150) < y && y < (150 + 1 * 150) = return $ AppState Motorway (addSlowdown exp 1 (x + 1000))
  | (10 + 0 * 150) < y && y < (150 + 0 * 150) = return $ AppState Motorway (addSlowdown exp 0 (x + 1000))
  | (10 - 1 * 150) < y && y < (150 - 1 * 150) = return $ AppState Motorway (addSlowdown exp (-1) (x + 1000))
  | (10 - 2.5 * 150) < y && y < (150 - 2.5 * 150) = return $ AppState Settings exp
  | (10 - 3.5 * 150) < y && y < (150 - 3.5 * 150) = exitSuccess
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
  let initState = AppState Settings (initExperiment gen)
  playIO display bgColor fps initState drawApp handleEvent updateApp
