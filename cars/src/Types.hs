module Types where

data Experiment =  Experiment
  { n :: Int -- количестов полос
  , rangeV :: (Int, Int) -- диапазон скоростей автомобилей
  , rangeT :: (Int, Int) -- интервал между появлениями автомобилей на дороге
  , deltaV :: Int -- величина уменьшения скорости искуственно приторм машины
  , deltaT :: Int -- время движения искуственно притормаживаемой машины с меньшей скоростью
  }

data Road = Road 
  { accidentT :: Int -- время аварии (длительность) 
  }

data RoadLane = RoadLane
  { number :: Int -- номер полосы
  , frequencyСar :: Int -- частота появления нового автомобиля
  , slowdown :: Int -- координата замедления
  }

data Car = Car
  { v0 :: Int -- начальная скорость
  , v :: Int -- текущая скоросоть
  , x :: Int -- текущая координата
  , status :: StatusCar -- состояние машины
  }

data StatusCar = Acceleration | Braking | ConstantSpeed | Accident
-- Ускорение | Торможение | Постоянная скорость | Авария