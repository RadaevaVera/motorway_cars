{-# LANGUAGE RecordWildCards #-}

module Experiment
  ( Experiment
  , initExperiment
  , newExperiment
  , oneStepExperiment
  , addSlowdown
  , getRoad
  , getSettings
  , addRangeV_L
  , subRangeV_L
  , addRangeT_L
  , subRangeT_L
  , addRangeV_R
  , subRangeV_R
  , addRangeT_R
  , subRangeT_R
  , addDeltaV
  , subDeltaV
  , addDeltaT
  , subDeltaT)
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

initExperiment :: StdGen -> Experiment
initExperiment gen = Experiment (10, 15) (60, 120) 1 180 (initRoad gen)

newExperiment :: Experiment -> StdGen -> Experiment
newExperiment Experiment{..} gen =
  Experiment rangeV rangeT deltaV deltaT (initRoad gen)

addSlowdown :: Experiment -> Float -> Float -> Experiment
addSlowdown Experiment{..} number x =
  Experiment
    rangeV
    rangeT
    deltaV
    deltaT
    $ addSlowdownOnRoad road number x

getSettings :: Experiment -> ((Int, Int), (Int, Int), Float, Float)
getSettings Experiment{..} = (rangeV, rangeT, deltaV, deltaT)

getRoad :: Experiment -> Road
getRoad Experiment{..} = road

addRangeV_L :: Experiment -> Experiment
addRangeV_L Experiment{..} =
  Experiment (addL 1 rangeV) rangeT deltaV deltaT road

subRangeV_L :: Experiment -> Experiment
subRangeV_L Experiment{..} =
  Experiment (subL 1 rangeV) rangeT deltaV deltaT road

addRangeT_L :: Experiment -> Experiment
addRangeT_L Experiment{..} =
  Experiment rangeV (addL 60 rangeT) deltaV deltaT road

subRangeT_L :: Experiment -> Experiment
subRangeT_L Experiment{..} =
  Experiment rangeV (subL 60 rangeT) deltaV deltaT road

addRangeV_R :: Experiment -> Experiment
addRangeV_R Experiment{..} =
  Experiment (addR 1 rangeV) rangeT deltaV deltaT road

subRangeV_R :: Experiment -> Experiment
subRangeV_R Experiment{..} =
  Experiment (subR 1 rangeV) rangeT deltaV deltaT road

addRangeT_R :: Experiment -> Experiment
addRangeT_R Experiment{..} =
  Experiment rangeV (addR 60 rangeT) deltaV deltaT road

subRangeT_R :: Experiment -> Experiment
subRangeT_R Experiment{..} =
  Experiment rangeV (subR 60 rangeT) deltaV deltaT road

addDeltaV :: Experiment -> Experiment
addDeltaV Experiment{..} =
  Experiment rangeV rangeT (deltaV + 1) deltaT road

subDeltaV :: Experiment -> Experiment
subDeltaV Experiment{..} =
  Experiment rangeV rangeT (deltaV - 1) deltaT road

addDeltaT :: Experiment -> Experiment
addDeltaT Experiment{..} =
  Experiment rangeV rangeT deltaV (deltaT + 60) road

subDeltaT :: Experiment -> Experiment
subDeltaT Experiment{..} =
  Experiment rangeV rangeT deltaV (deltaT - 60) road


addL n (r1, r2) = (r1 + n, r2)
subL n (r1, r2) = (r1 - n, r2)
addR n (r1, r2) = (r1, r2 + n)
subR n (r1, r2) = (r1, r2 - n)
