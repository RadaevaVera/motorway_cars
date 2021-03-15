{-# LANGUAGE RecordWildCards #-}

module Road
    ( Road(..)
    , drawRoad
    , moveCarsOnRoad
    , renderCarsOnRoad
    , removeCarsOnRoad
    , addSlowdownOnRoad)
    where

import Graphics.Gloss.Interface.IO.Game
import RoadLane
import System.Random

data Road = Road
  { randomGen :: StdGen
  , accidentT :: Float -- время аварии (длительность)
  , lateCarT :: [Int] -- сколько времени назад появилась машина в каждой полосе
  , roadLane :: [RoadLane]
  }

drawRoad :: Road -> Picture
drawRoad Road{..} = Pictures $ drawButtom : (map drawRoadLane roadLane)

drawButtom :: Picture
drawButtom = Pictures $
  [Color (light $ light red) $
    polygon [(-1000, 10 - 3.5 * 150),
             (-1000, 150 - 3.5 * 150),
             (1000, 150 - 3.5 * 150),
             (1000, 10 - 3.5 * 150)]
    , translate (-900) (-490) . scale 0.8 0.8 . color black . text $ "CHANGE SIMULATION PARAMETERS"]

moveCarsOnRoad :: Road -> Float -> Float -> Road
moveCarsOnRoad Road{..} dV dT =
  Road
    randomGen
    accidentT
    lateCarT
    (map (\ x -> moveCarsOnRoadLane x accidentT dV dT) roadLane)

renderCarsOnRoad :: Road -> (Int, Int) -> (Int, Int) -> Road
renderCarsOnRoad Road{..} rangeV rangeT  = subLateCarT $
  renderCars Road{..} rangeV rangeT (toCount lateCarT [1,0,-1,-2])
  where
    toCount [] _ = []
    toCount _ [] = []
    toCount (x:xs) (n:ns) = [(x,n)] ++ toCount xs ns

removeCarsOnRoad :: Road -> Road
removeCarsOnRoad Road{..} =
  Road
    randomGen
    accidentT
    lateCarT
    $ map removeCarsOnRoadLane roadLane

addSlowdownOnRoad :: Road -> Float -> Float -> Road
addSlowdownOnRoad Road{..} number x =
  Road
    randomGen
    accidentT
    lateCarT
    $ map (\ r -> addSlowdownOnRoadLane r number x) roadLane

renderCars :: Road -> (Int, Int) -> (Int, Int) -> [(Int , Float)] -> Road
renderCars road _ _ [] = road
renderCars (Road randomGen0 accidentT late roadLane) rangeV rangeT ((0, n):xs) =
    let (newT, newGen1) = randomR rangeT randomGen0
        (newV, randomGen) = randomR rangeV newGen1
        lateCarT = (newLateCarT late n newT)
        roadNew = renderCar Road{..} n (fromIntegral newV)
    in renderCars roadNew rangeV rangeT xs
renderCars road rangeV rangeT (x:xs) = renderCars road rangeV rangeT xs

renderCar :: Road -> Float -> Float -> Road
renderCar Road{..} n newV = Road
  randomGen
  accidentT
  lateCarT
  (map (\ x -> renderCarOnRoadLane x n newV) roadLane)

subLateCarT :: Road -> Road
subLateCarT (Road randomGen accidentT late roadLane) =
    let
      subT 0 = 0
      subT n = n - 1
      lateCarT = map subT late
    in Road{..}

newLateCarT :: [Int] -> Float -> Int -> [Int]
newLateCarT (l:late) 1 newT = (newT:late)
newLateCarT (l1:l2:late) 0 newT = (l1:newT:late)
newLateCarT (l1:l2:l3:late) (-1) newT = (l1:l2:newT:late)
newLateCarT (l1:l2:l3:late) (-2) newT = (l1:l2:l3:newT:late)
newLateCarT late _ _ = late