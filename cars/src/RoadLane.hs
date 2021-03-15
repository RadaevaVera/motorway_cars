{-# LANGUAGE RecordWildCards #-}

module RoadLane
    ( RoadLane(..)
    , moveCarsOnRoadLane
    , renderCarOnRoadLane
    , removeCarsOnRoadLane
    , addSlowdownOnRoadLane
    , drawRoadLane)
    where

import Car
import Graphics.Gloss.Interface.IO.Game

data RoadLane = RoadLane
  { number :: Float -- номер полосы [-2, 1]
  , slowdown :: [Float] -- координаты замедления
  , car :: [Car]
  }

moveCarsOnRoadLane :: RoadLane -> Float -> Float -> Float -> RoadLane
moveCarsOnRoadLane RoadLane{..} accidentT deltaV deltaT = do
  let
    newSlowdown = map (\ x -> slowdownCar x slowdown deltaV) car
    movedCars = map moveCar newSlowdown
    resultCars = changeCarStatus movedCars accidentT deltaT
    resultSlowdown = removeUsedSlowdown slowdown car
  RoadLane number resultSlowdown resultCars

renderCarOnRoadLane :: RoadLane -> Float -> Float -> RoadLane
renderCarOnRoadLane RoadLane{..} n v =
  if (n == number)
    then RoadLane number slowdown (askRender car (Car v v (-500) 0 ConstantSpeed))
    else RoadLane{..}

removeCarsOnRoadLane :: RoadLane -> RoadLane
removeCarsOnRoadLane RoadLane{..} =
  RoadLane
    number
    slowdown
    (removeCars car)

addSlowdownOnRoadLane :: RoadLane -> Float -> Float -> RoadLane
addSlowdownOnRoadLane RoadLane{..} n x
  | n == number = RoadLane number (x:slowdown) car
  | otherwise = RoadLane{..}

drawRoadLane :: RoadLane -> Picture
drawRoadLane RoadLane{..} = Pictures $
  [Color (dark white) $ polygon [(-1000, 10 + number * 150),
            (-1000, 150 + number * 150),
            (1000, 150 + number * 150),
            (1000, 10 + number *150)]]
  ++ map (drawSlowdown number) slowdown
  ++ map (\x -> drawCar x number) car

drawSlowdown :: Float -> Float -> Picture
drawSlowdown number x =
  Color white $ polygon [(-1000 + x, 70 + number * 150),
                         (-1000 + x, 90 + number * 150),
                         (-980 + x, 90 + number * 150),
                         (-980 + x, 70 + number *150)]

changeCarStatus :: [Car] -> Float -> Float -> [Car]
changeCarStatus [] _ _ = []
changeCarStatus [(Car v0 v x t status)] accidentT deltaT
  | status == Accident && t < accidentT = end
  | status == ArtificiallySlowdown && t < deltaT = end
  | status == ConstantSpeed = end
  | otherwise = [(Car v0 v x 0 Acceleration)]
  where end = [(Car v0 v x t status)]

changeCarStatus ((Car v0 v x t Accident):(Car v01 v1 x1 t1 status1):xs) accidentT deltaT
  | (status1 == Accident) = (Car v0 v x t Accident):end
  | accidentT <= t && (x1 - x) >= 200 = (Car v0 v x 0 Acceleration):end
  | otherwise = (Car v0 v x t Accident):end
  where end = (changeCarStatus ((Car v01 v1 x1 t1 status1):xs) accidentT deltaT)

changeCarStatus ((Car v0 v x t status):(Car v01 v1 x1 t1 status1):xs) accidentT deltaT
  | (x1 - x) <= 160 = (Car v0 0 x t Accident):(Car v01 0 x1 t1 Accident):(changeCarStatus xs accidentT deltaT)
  | (x1 - x) <= 500 && v > v1 = (Car v0 v x t Braking):end
  | status == ArtificiallySlowdown && deltaT > t = (Car v0 v x t ArtificiallySlowdown):end
  | status == ArtificiallySlowdown && deltaT <= t = (Car v0 v x 0 Acceleration):end
  | (x1 - x) > 500 && v0 > v = (Car v0 v x t Acceleration):end
  | (x1 - x) > 500 && v0 <= v = (Car v0 v0 x t ConstantSpeed):end
  | status == Braking && v > (v1 + 1) = (Car v0 v x t Braking):end
  | status == Braking && v <= v1 = (Car v0 v1 x t ConstantSpeed):end
  | otherwise = (Car v0 v x t status): end
  where end = (changeCarStatus ((Car v01 v1 x1 t1 status1):xs) accidentT deltaT)

askRender :: [Car] -> Car -> [Car]
askRender [] newCar = [newCar]
askRender ((Car v0 v x t status):xs) newCar
  | x <= 0 = (Car v0 v x t status):xs
  | otherwise = newCar:(Car v0 v x t status):xs

removeCars :: [Car] -> [Car]
removeCars [] = []
removeCars (car:xs) = do
  let mbCar = removeCar car
  case mbCar of
    Just x -> x : (removeCars xs)
    Nothing -> removeCars xs

removeUsedSlowdown :: [Float] -> [Car] -> [Float]
removeUsedSlowdown sl [] = sl
removeUsedSlowdown sl (car:xs) =
  removeUsedSlowdown (removeUsedSlowdown2 sl car) xs

removeUsedSlowdown2 :: [Float] -> Car -> [Float]
removeUsedSlowdown2 [] _ = []
removeUsedSlowdown2 (s:ss) Car{..}
  | abs (s - (x + 50)) <= 50 = removeUsedSlowdown2 ss Car{..}
  | otherwise = s : (removeUsedSlowdown2 ss Car{..})