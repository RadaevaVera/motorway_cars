{-# LANGUAGE RecordWildCards #-}

module GameIO
    ( runGame
    ) where

import Types
import Draw (drawRoad, drawSettings)

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap
import System.IO
import System.Random
import Data.List

------------------
--Отриcовка-------
------------------

display :: Display
display = InWindow "cars" (500, 500) (-500, -500)

bgColor :: Color
bgColor = white

drawApp :: Experiment -> IO Picture
drawApp Experiment{..}
  | (appState == Settings) = return $ drawSettings
  | otherwise = return $ drawRoad road

--------------------
--Нажатие клавиш----
--------------------

handleEvent :: Event -> Experiment -> IO Experiment
handleEvent _ state = return state

-----------------------------------------------------------------
--Обработка состояния (проиcходящее без нажатия клавиш)----------
-----------------------------------------------------------------

updateApp :: Float -> Experiment -> IO Experiment
updateApp _ state = return state

--------------------------------------------------------

fps :: Int
fps = 1

runGame :: IO ()
runGame = do
  gen <- getStdGen
  let
    car1 = Car 1 1 100 Acceleration
    car2 = Car 1 1 600 Braking
    car3 = Car 1 1 400 ConstantSpeed
    car4 = Car 1 1 800 Accident
    roadLane1 = RoadLane 1 10 [100] [car1, car2]
    roadLane2 = RoadLane 0 0 [900] [car3]
    roadLane3 = RoadLane (-1) 200 [200] [car4]
    initRoad = Road 5 [roadLane1, roadLane2, roadLane3]
    initState = Experiment Motorway (40, 70) (3, 5) 10 5 initRoad
  playIO display bgColor fps initState drawApp handleEvent updateApp
