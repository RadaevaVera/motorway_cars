{-# LANGUAGE RecordWildCards #-}

module RoadLane where

import Car
import Graphics.Gloss.Interface.IO.Game

data RoadLane = RoadLane
  { number :: Float -- номер полосы [-2, 1]
  , slowdown :: [Float] -- координаты замедления
  , car :: [Car]
  }

moveCarsOnRoadLane :: RoadLane -> Int -> RoadLane
moveCarsOnRoadLane RoadLane{..} accidentT =
  RoadLane
    number
    slowdown
    (map ((changeStatus 0 0).moveCar) car)

renderCarOnRoadLane :: RoadLane -> Float -> Float -> RoadLane
renderCarOnRoadLane RoadLane{..} n v =
  if (n == number)
    then RoadLane number slowdown ((Car v v (-150) 0 ConstantSpeed):car)
    else RoadLane{..}

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
