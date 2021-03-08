{-# LANGUAGE RecordWildCards #-}

module Draw where

import Types
import Data.List
import Graphics.Gloss.Interface.IO.Game

drawRoad :: Road -> Picture
drawRoad Road{..} = Pictures $ map drawRoadLane roadLane

drawRoadLane :: RoadLane -> Picture
drawRoadLane RoadLane{..} = Pictures $
  [Color (dark white) $ polygon [(-1000, 10 + number * 150),
            (-1000, 150 + number * 150),
            (1000, 150 + number * 150),
            (1000, 10 + number *150)]]
  ++ map (drawSlowdown number) slowdown
  ++ map (drawCar number) car

drawSlowdown :: Float -> Float -> Picture
drawSlowdown number x =
  Color white $ polygon [(-1000 + x, 70 + number * 150),
                         (-1000 + x, 90 + number * 150),
                         (-980 + x, 90 + number * 150),
                         (-980 + x, 70 + number *150)]

drawCar :: Float -> Car -> Picture
drawCar number Car{..}
  | (status == Acceleration) = drawCar2 green x number
  | (status == Braking) = drawCar2 red x number
  | (status == ConstantSpeed) = drawCar2 blue x number
  | otherwise = drawCar2 black x number

drawCar2 :: Color -> Float -> Float -> Picture
drawCar2 color x number = Pictures $
  -- корпус
  [ Color (dim color) $ polygon [(-1000 + x, 50 + number * 150),
                          (-1000 + x, 110 + number * 150),
                          (-850 + x, 110 + number * 150),
                          (-850 + x, 50 + number *150)]
  -- крыша
  , Color (light $ light color) $
                 polygon [(-960 + x, 60 + number * 150),
                          (-960 + x, 100 + number * 150),
                          (-890 + x, 100 + number * 150),
                          (-890 + x, 60 + number *150)]
  -- вередние фонари
  , Color (light yellow) $
                  polygon [(-850 + x, 55 + number * 150),
                            (-850 + x, 65 + number * 150),
                            (-860 + x, 65 + number * 150),
                            (-860 + x, 55 + number *150)]
  , Color (light yellow) $
                  polygon [(-850 + x, 95 + number * 150),
                            (-850 + x, 105 + number * 150),
                            (-860 + x, 105 + number * 150),
                            (-860 + x, 95 + number *150)]
    -- задние фонари
  , Color rose $   polygon [(-990 + x, 55 + number * 150),
                            (-990 + x, 65 + number * 150),
                            (-1000 + x, 65 + number * 150),
                            (-1000 + x, 55 + number *150)]
  , Color rose $   polygon [(-990 + x, 95 + number * 150),
                            (-990 + x, 105 + number * 150),
                            (-1000 + x, 105 + number * 150),
                            (-1000 + x, 95 + number *150)]]

drawSettings :: Picture
drawSettings =
  polygon [(10,10), (10,-10),(-10,-10), (-10,10)]