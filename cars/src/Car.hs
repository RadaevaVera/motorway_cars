{-# LANGUAGE RecordWildCards #-}

module Car
    ( Car(..)
    , StatusCar(..)
    , moveCar
    , slowdownCar
    , removeCar
    , drawCar
    , compareCar)
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
  | abs (s - (x + 50)) <= 50 && (v - deltaV) >= 0 = Car v0 (v - deltaV) x 0 ArtificiallySlowdown
  | abs (s - (x + 50)) <= 50 = Car v0 0 x 0 ArtificiallySlowdown
  | otherwise = Car{..}

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
compareCar Car{..} (Car v01 v1 x1 t1 status1) =
  if (status /= Accident && status1 == Accident)
    then 1
    else 0
