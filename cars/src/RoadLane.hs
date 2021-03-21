{-# LANGUAGE RecordWildCards #-}

module RoadLane
    ( RoadLane
    , moveCarsOnRoadLane
    , renderCarOnRoadLane
    , removeCarsOnRoadLane
    , addSlowdownOnRoadLane
    , drawRoadLane
    , getNumberAccidentOnRoadLane
    , initRoadLane)
    where

import Car
import Graphics.Gloss.Interface.IO.Game

data RoadLane = RoadLane
  { number :: Float -- номер полосы [-2, 1]
  , slowdown :: [Float] -- координаты замедления
  , accident :: Int -- количество аварий
  , car :: [Car]
  }

moveCarsOnRoadLane :: RoadLane -> Float -> Float -> Float -> RoadLane
moveCarsOnRoadLane RoadLane{..} accidentT deltaV deltaT = do
  let
    newSlowdown = map (\ x -> slowdownCar x slowdown deltaV) car
    movedCars = map moveCar newSlowdown
    resultCars = changeCarStatus movedCars accidentT deltaT
    resultSlowdown = removeUsedSlowdown slowdown car
    accidentNew = accident + (compareCars car resultCars)
  RoadLane number resultSlowdown accidentNew resultCars

renderCarOnRoadLane :: RoadLane -> Float -> Float -> RoadLane
renderCarOnRoadLane RoadLane{..} n v =
  if (n == number)
    then RoadLane number slowdown accident (askRender car (getNewCar v))
    else RoadLane{..}

removeCarsOnRoadLane :: RoadLane -> RoadLane
removeCarsOnRoadLane RoadLane{..} =
  RoadLane
    number
    slowdown
    accident
    (removeCars car)

addSlowdownOnRoadLane :: RoadLane -> Float -> Float -> RoadLane
addSlowdownOnRoadLane RoadLane{..} n x
  | n == number = RoadLane number (x:slowdown) accident car
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
changeCarStatus [car] accidentT deltaT = [changeLastCarStatus car accidentT deltaT]
changeCarStatus (car1:car2:xs) accidentT deltaT =
  let (carNew1, carNew2, fl) = changeTwoCarStatus car1 car2 accidentT deltaT
  in if (fl) then carNew1 : (changeCarStatus (carNew2:xs) accidentT deltaT)
             else carNew1 : carNew2 : (changeCarStatus xs accidentT deltaT)

askRender :: [Car] -> Car -> [Car]
askRender [] newCar = [newCar]
askRender (car:xs) newCar
  | getX car <= 0 = car:xs
  | otherwise = newCar:car:xs

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
removeUsedSlowdown2 (s:ss) car
  | abs (s - ((getX car) + 50)) <= 50 = removeUsedSlowdown2 ss car
  | otherwise = s : (removeUsedSlowdown2 ss car)

getNumberAccidentOnRoadLane :: RoadLane -> Int
getNumberAccidentOnRoadLane RoadLane{..} = accident

compareCars :: [Car] -> [Car] -> Int
compareCars car newCar = foldr (+) 0 (zipWith compareCar car newCar)

initRoadLane :: Float -> RoadLane
initRoadLane number = RoadLane number [] 0 []
