{-# LANGUAGE RecordWildCards #-}

module RenderCars where

import Data.List
import System.Random
import Types

renderCars :: Experiment -> Experiment
renderCars Experiment{..} = subLateCarT $
  renderCars2 Experiment{..} (toCount lateCarT [1,0,-1,-2])
  where
    toCount [] _ = []
    toCount (x:xs) (n:ns) = [(x,n)] ++ toCount xs ns


renderCars2 :: Experiment -> [(Int , Float)] -> Experiment
renderCars2 experiment [] = experiment
renderCars2 (Experiment randomGen0 appState rangeV
  rangeT late deltaV deltaT road0)((0, n):xs) =
    let (newT, newGen1) = randomR rangeT randomGen0
        (newV, randomGen) = randomR rangeV newGen1
        road = renderCar road0 n (fromIntegral newV)
        lateCarT = (newLateCarT late n newT)
    in renderCars2 Experiment{..} xs
renderCars2 experiment (x:xs) = renderCars2 experiment xs

newLateCarT :: [Int] -> Float -> Int -> [Int]
newLateCarT (l:late) 1 newT = (newT:late)
newLateCarT (l1:l2:late) 0 newT = (l1:newT:late)
newLateCarT (l1:l2:l3:late) (-1) newT = (l1:l2:newT:late)
newLateCarT (l1:l2:l3:late) (-2) newT = (l1:l2:l3:newT:late)
newLateCarT late _ _ = late

subLateCarT :: Experiment -> Experiment
subLateCarT (Experiment randomGen appState rangeV
  rangeT late deltaV deltaT road) =
    let
      subT 0 = 0
      subT n = n - 1
      lateCarT = map subT late
    in Experiment{..}

renderCar :: Road -> Float -> Float -> Road
renderCar Road{..} n newV =
  Road accidentT (map (renderCar2 n newV) roadLane)

renderCar2 :: Float -> Float -> RoadLane -> RoadLane
renderCar2 n v RoadLane{..} =
  if (n == number)
    then RoadLane number slowdown ((Car v v (-150) 0 ConstantSpeed):car)
    else RoadLane{..}