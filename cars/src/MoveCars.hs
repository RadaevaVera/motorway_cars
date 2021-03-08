{-# LANGUAGE RecordWildCards #-}

module MoveCars where

import Data.List
import System.IO
import System.Random
import Types

moveCars :: Experiment -> Experiment
moveCars Experiment{..} = Experiment randomGen appState rangeV
  rangeT lateCarT deltaV deltaT (moveCars2 road deltaV deltaT)

moveCars2 :: Road -> Float -> Float -> Road
moveCars2 Road{..} deltaV deltaT =
  Road accidentT (map (moveCars3 accidentT) roadLane)

moveCars3 :: Int -> RoadLane -> RoadLane
moveCars3 accidentT RoadLane{..} = RoadLane number slowdown
  (map ((changeStatus 0 0).moveCar) car)

changeStatus :: Int -> Int -> Car -> Car
changeStatus _ _ Car{..} = Car{..}


moveCar :: Car -> Car
moveCar Car{..}
  | (status == Acceleration) = acceleration Car{..}
  | (status == Braking) = braking Car{..}
  | (status == Accident) = accident Car{..}
  | otherwise = constantSpeed Car{..}

acceleration :: Car -> Car
acceleration Car{..} = Car v0 (v + 6) (x + v - 3) t status

braking :: Car -> Car
braking Car{..} = Car v0 (v - 8) (x + v - 4) t status

constantSpeed :: Car -> Car
constantSpeed Car{..} = Car v0 v (x + v) t status

accident :: Car -> Car
accident Car{..} = Car v0 0 x (t + 1) status


