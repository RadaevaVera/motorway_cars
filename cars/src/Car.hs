{-# LANGUAGE RecordWildCards #-}

module Car
    ( Car
    , getX
    , moveCar
    , slowdownCar
    , removeCar
    , drawCar
    , compareCar
    , getNewCar
    , changeLastCarStatus
    , changeTwoCarStatus)
    where

import Graphics.Gloss.Interface.IO.Game

data Car = Car
  { v0 :: Float -- начальная скорость
  , v :: Float -- текущая скоросоть
  , x :: Float -- текущая координата
  , t :: Float -- время простоя в аварии или искуственное приторможение
  , status :: StatusCar -- состояние машины
  }

data StatusCar = Acceleration | Braking | ConstantSpeed | Accident | ArtificiallySlowdown deriving (Eq)
-- Ускорение | Торможение | Постоянная скорость | Авария

moveCar :: Car -> Car
moveCar Car{..}
  | (status == Acceleration) = acceleration Car{..}
  | (status == Braking) = braking Car{..}
  | (status == Accident) = accident Car{..}
  | (status == ArtificiallySlowdown) = artificiallySlowdown Car{..}
  | otherwise = constantSpeed Car{..}

slowdownCar :: Car -> [Float] -> Float -> Car
slowdownCar car [] _ = car
slowdownCar Car{..} (s:ss) deltaV
  | abs (s - (x + 50)) <= 50 && (v - deltaV) >= 0
    = slowdownCar (Car v0 (v - deltaV) x 0 ArtificiallySlowdown) ss deltaV
  | abs (s - (x + 50)) <= 50
    = Car v0 0 x 0 ArtificiallySlowdown
  | otherwise
    = slowdownCar Car{..} ss deltaV

removeCar :: Car -> Maybe Car
removeCar Car{..}
  | x > 2500 = Nothing
  | otherwise = Just Car{..}

drawCar :: Car -> Float -> Picture
drawCar Car{..} number
  | (status == Acceleration) = drawCar2 green x number
  | (status == Braking) = drawCar2 red x number
  | (status == ConstantSpeed) = drawCar2 blue x number
  | (status == ArtificiallySlowdown) = drawCar2 violet x number
  | otherwise = drawCar2 black x number

drawCar2 :: Color -> Float -> Float -> Picture
drawCar2 carColor x number = Pictures $
  -- корпус
  [ Color (dim carColor) $
                 polygon [(-1000 + x, 50 + number * 150),
                          (-1000 + x, 110 + number * 150),
                          (-850 + x, 110 + number * 150),
                          (-850 + x, 50 + number *150)]
  -- крыша
  , Color (light $ light carColor) $
                 polygon [(-960 + x, 60 + number * 150),
                          (-960 + x, 100 + number * 150),
                          (-890 + x, 100 + number * 150),
                          (-890 + x, 60 + number *150)]
  -- передние фонари
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

acceleration :: Car -> Car
acceleration Car{..} = Car v0 (v + 0.08) (x + v - 0.04) 0 status

braking :: Car -> Car
braking Car{..}
  |(v - 0.16 >= 0) = Car v0 (v - 0.16) (x + v - 0.08) 0 status
  |otherwise = Car v0 0 x t status

constantSpeed :: Car -> Car
constantSpeed Car{..} = Car v0 v (x + v) 0 status

accident :: Car -> Car
accident Car{..} = Car v0 0 x (t + 1) status

artificiallySlowdown :: Car -> Car
artificiallySlowdown Car{..} = Car v0 v (x + v) (t + 1) status

compareCar :: Car -> Car -> Int
compareCar Car{..} (Car _ _ _ _ status1) =
  if (status /= Accident && status1 == Accident)
    then 1
    else 0

getX :: Car -> Float
getX Car{..} = x

getNewCar :: Float -> Car
getNewCar v = Car v v (-500) 0 ConstantSpeed

changeLastCarStatus :: Car -> Float -> Float -> Car
changeLastCarStatus Car{..} accidentT deltaT
  | status == Accident && t < accidentT = Car{..}
  | status == ArtificiallySlowdown && t < deltaT = Car{..}
  | status == ConstantSpeed = Car{..}
  | otherwise = Car v0 v x 0 Acceleration

changeTwoCarStatus :: Car -> Car -> Float -> Float -> (Car, Car, Bool)
changeTwoCarStatus (Car v0 v x t Accident) (Car v01 v1 x1 t1 status1) accidentT _
  | (status1 == Accident) = (Car v0 v x t Accident , car2 , True)
  | accidentT <= t && (x1 - x) >= 200 = (Car v0 v x 0 Acceleration , car2 , True)
  | otherwise = (Car v0 v x t Accident , car2, True)
  where car2 = Car v01 v1 x1 t1 status1
changeTwoCarStatus (Car v0 v x t status) (Car v01 v1 x1 t1 status1) _ deltaT
  | (x1 - x) <= 160 = (Car v0 0 x t Accident , Car v01 0 x1 t1 Accident, False)
  | (x1 - x) <= 500 && v > v1 = (Car v0 v x t Braking , car2 , True)
  | status == ArtificiallySlowdown && deltaT > t = (Car v0 v x t ArtificiallySlowdown , car2 , True)
  | status == ArtificiallySlowdown && deltaT <= t = (Car v0 v x 0 Acceleration , car2 , True)
  | (x1 - x) > 500 && v0 > v = (Car v0 v x t Acceleration , car2 , True)
  | (x1 - x) > 500 && v0 <= v = (Car v0 v0 x t ConstantSpeed , car2 , True)
  | status == Braking && v > (v1 + 1) = (Car v0 v x t Braking , car2 , True)
  | status == Braking && v <= v1 = (Car v0 v1 x t ConstantSpeed , car2 , True)
  | otherwise = (Car v0 v x t status , car2 , True)
  where car2 = (Car v01 v1 x1 t1 status1)
