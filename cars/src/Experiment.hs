{-# LANGUAGE RecordWildCards #-}

module Experiment where

import Road
import System.Random

data Experiment =  Experiment
  { rangeV :: (Int, Int) -- диапазон скоростей автомобилей
  , rangeT :: (Int, Int) -- интервал между появлениями автомобилей на дороге
  , deltaV :: Float -- величина уменьшения скорости искуственно приторм машины
  , deltaT :: Float -- время движения искуственно притормаживаемой машины с меньшей скоростью
  , road :: Road
  }

oneStepExperiment :: Experiment -> Experiment
oneStepExperiment Experiment{..} =
  let
    movedRoad = moveCarsOnRoad road deltaV deltaT
    resultRoad = renderCarsOnRoad movedRoad rangeV rangeT
  in
    Experiment
      rangeV
      rangeT
      deltaV
      deltaT
      resultRoad
