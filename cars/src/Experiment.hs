{-# LANGUAGE RecordWildCards #-}

module Experiment
  ( Experiment(..)
  , oneStepExperiment
  , addSlowdown)
  where

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
    renderedRoad = renderCarsOnRoad movedRoad rangeV rangeT
    resultRoad = removeCarsOnRoad renderedRoad
  in
    Experiment
      rangeV
      rangeT
      deltaV
      deltaT
      resultRoad

addSlowdown :: Experiment -> Float -> Float -> Experiment
addSlowdown Experiment{..} number x =
  Experiment
    rangeV
    rangeT
    deltaV
    deltaT
    $ addSlowdownOnRoad road number x
