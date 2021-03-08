module Types where

import System.Random

data Experiment =  Experiment
  { randomGen :: StdGen
  , appState :: AppState
  , rangeV :: (Int, Int) -- диапазон скоростей автомобилей
  , rangeT :: (Int, Int) -- интервал между появлениями автомобилей на дороге
  , lateCarT :: [Int] -- сколько времени назад появилась машина в каждой полосе
  , deltaV :: Float -- величина уменьшения скорости искуственно приторм машины
  , deltaT :: Float -- время движения искуственно притормаживаемой машины с меньшей скоростью
  , road :: Road
  }

data Road = Road
  { accidentT :: Int -- время аварии (длительность)
  , roadLane :: [RoadLane]
  }

data RoadLane = RoadLane
  { number :: Float -- номер полосы [-2, 1]
  , slowdown :: [Float] -- координаты замедления
  , car :: [Car]
  }

data Car = Car
  { v0 :: Float -- начальная скорость
  , v :: Float -- текущая скоросоть
  , x :: Float -- текущая координата
  , t :: Float -- время простоя в аварии или приторможение
  , status :: StatusCar -- состояние машины
  }

data StatusCar = Acceleration | Braking | ConstantSpeed | Accident | ArtificiallySlowdown deriving (Eq)
-- Ускорение | Торможение | Постоянная скорость | Авария

data AppState = Settings | Motorway deriving (Eq)
