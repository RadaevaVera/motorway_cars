module Types where

data Experiment =  Experiment
  { appState :: AppState
  , rangeV :: (Int, Int) -- диапазон скоростей автомобилей
  , rangeT :: (Int, Int) -- интервал между появлениями автомобилей на дороге
  , deltaV :: Int -- величина уменьшения скорости искуственно приторм машины
  , deltaT :: Int -- время движения искуственно притормаживаемой машины с меньшей скоростью
  , road :: Road
  }

data Road = Road
  { accidentT :: Int -- время аварии (длительность)
  , roadLane :: [RoadLane]
  }

data RoadLane = RoadLane
  { number :: Float -- номер полосы [-2, 1]
  , frequencyСar :: Int -- частота появления нового автомобиля
  , slowdown :: [Float] -- координаты замедления
  , car :: [Car]
  }

data Car = Car
  { v0 :: Int -- начальная скорость
  , v :: Int -- текущая скоросоть
  , x :: Float -- текущая координата
  , status :: StatusCar -- состояние машины
  }

data StatusCar = Acceleration | Braking | ConstantSpeed | Accident deriving (Eq)
-- Ускорение | Торможение | Постоянная скорость | Авария

data AppState = Settings | Motorway deriving (Eq)
