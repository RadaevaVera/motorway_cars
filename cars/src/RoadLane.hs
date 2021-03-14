{-# LANGUAGE RecordWildCards #-}

module RoadLane
    ( RoadLane(..)
    , moveCarsOnRoadLane
    , renderCarOnRoadLane
    , removeCarsOnRoadLane
    , drawRoadLane)
    where

import Car
import Graphics.Gloss.Interface.IO.Game

data RoadLane = RoadLane
  { number :: Float -- номер полосы [-2, 1]
  , slowdown :: [Float] -- координаты замедления
  , car :: [Car]
  }

moveCarsOnRoadLane :: RoadLane -> Float -> RoadLane
moveCarsOnRoadLane RoadLane{..} accidentT =
  RoadLane
    number
    slowdown
    (changeCarStatus (map moveCar car) accidentT)

renderCarOnRoadLane :: RoadLane -> Float -> Float -> RoadLane
renderCarOnRoadLane RoadLane{..} n v =
  if (n == number)
    then RoadLane number slowdown (askRender car (Car v v (-300) 0 ConstantSpeed))
    else RoadLane{..}

removeCarsOnRoadLane :: RoadLane -> RoadLane
removeCarsOnRoadLane RoadLane{..} =
  RoadLane
    number
    slowdown
    (removeCars car)

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

changeCarStatus :: [Car] -> Float -> [Car]
changeCarStatus [] _ = []
changeCarStatus [(Car v0 v x t status)] accidentT
  | status == Accident && t < accidentT = [(Car v0 v x t Accident)]
  | status == ConstantSpeed = [(Car v0 v x t status)]
  | otherwise = [(Car v0 v x t Acceleration)]
changeCarStatus ((Car v0 v x t Accident):(Car v01 v1 x1 t1 status1):xs) accidentT
  | (status1 == Accident) = (Car v0 v x t Accident):(changeCarStatus ((Car v01 v1 x1 t1 status1):xs) accidentT)
  | accidentT <= t && (x1 - x) >= 200 = (Car v0 v x 0 Acceleration):(changeCarStatus ((Car v01 v1 x1 t1 status1):xs) accidentT)
  | otherwise = (Car v0 v x t Accident):(changeCarStatus ((Car v01 v1 x1 t1 status1):xs) accidentT)
changeCarStatus ((Car v0 v x t status):(Car v01 v1 x1 t1 status1):xs) aT
  | (x1 - x) <= 160 = (Car v0 0 x t Accident):(Car v01 0 x1 t1 Accident):(changeCarStatus xs aT)
  | (x1 - x) <= 350 && v > v1 = (Car v0 v x t Braking):(changeCarStatus ((Car v01 v1 x1 t1 status1):xs) aT)
  | (x1 - x) > 350 && v0 > v = (Car v0 v x t Acceleration):(changeCarStatus ((Car v01 v1 x1 t1 status1):xs) aT)
  | (x1 - x) > 350 && v0 <= v = (Car v0 v0 x t ConstantSpeed):(changeCarStatus ((Car v01 v1 x1 t1 status1):xs) aT)
  | status == Braking && v > (v1 + 1) = (Car v0 v x t Braking):(changeCarStatus ((Car v01 v1 x1 t1 status1):xs) aT)
  | status == Braking && v <= v1 = (Car v0 v1 x t ConstantSpeed):(changeCarStatus ((Car v01 v1 x1 t1 status1):xs) aT)
  | otherwise = (Car v0 v x t status):(changeCarStatus ((Car v01 v1 x1 t1 status1):xs) aT)

askRender :: [Car] -> Car -> [Car]
askRender [] newCar = [newCar]
askRender ((Car v0 v x t status):xs) newCar
  | x <= 100 = (Car v0 v x t status):xs
  | otherwise = newCar:(Car v0 v x t status):xs

removeCars :: [Car] -> [Car]
removeCars [] = []
removeCars (car:xs) = do
  let mbCar = removeCar car
  case mbCar of
    Just x -> x : (removeCars xs)
    Nothing -> removeCars xs
