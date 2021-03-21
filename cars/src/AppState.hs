{-# LANGUAGE RecordWildCards #-}

module AppState
  ( AppState
  , initState
  , eventAppState
  , updateAppState
  , drawAppState)
  where

import Experiment
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)
import System.Random

data AppState = AppState
  { page :: Page
  , experiment :: Experiment
  }

data Page = Settings | Motorway deriving (Eq)

drawAppState :: AppState -> Picture
drawAppState (AppState page exp)
  | (page == Settings) = drawSettings (getSettings exp)
  | otherwise = Pictures $ [drawButtoms] ++ (drawExperiment exp)

initState :: StdGen -> AppState
initState gen = AppState Settings (initExperiment gen)

updateAppState :: AppState -> AppState
updateAppState AppState{..}
  | page == Motorway = AppState page (oneStepExperiment experiment)
  | otherwise = AppState{..}

eventAppState :: AppState  -> Float -> Float -> IO AppState
eventAppState (AppState Settings exp) x y
  | (10 - 2.5 * 150) < y && y < (150 - 2.5 * 150) = do
    gen <- getStdGen
    return $ AppState Motorway (newExperiment exp gen)
  | (10 - 3.5 * 150) < y && y < (150 - 3.5 * 150)
    = exitSuccess
  | (90 + 2 * 150) < y && y < (120 + 2 * 150) && 500 < x && x < 540
    = return $ AppState Settings (addRangeV_L exp)
  | (40 + 2 * 150) < y && y < (70 + 2 * 150) && 500 < x && x < 540
    = return $ AppState Settings (subRangeV_L exp)
  | (90 + 1 * 150) < y && y < (120 + 1 * 150) && 500 < x && x < 540
    = return $ AppState Settings (addRangeT_L exp)
  | (40 + 1 * 150) < y && y < (70 + 1 * 150) && 500 < x && x < 540
    = return $ AppState Settings (subRangeT_L exp)
  | (90 + 2 * 150) < y && y < (120 + 2 * 150) && 710 < x && x < 750
    = return $ AppState Settings (addRangeV_R exp)
  | (40 + 2 * 150) < y && y < (70 + 2 * 150) && 710 < x && x < 750
    = return $ AppState Settings (subRangeV_R exp)
  | (90 + 1 * 150) < y && y < (120 + 1 * 150) && 710 < x && x < 750
    = return $ AppState Settings (addRangeT_R exp)
  | (40 + 1 * 150) < y && y < (70 + 1 * 150) && 710 < x && x < 750
    = return $ AppState Settings (subRangeT_R exp)
  | (90 + 0 * 150) < y && y < (120 + 0 * 150) && 500 < x && x < 540
    = return $ AppState Settings (addDeltaV exp)
  | (40 + 0 * 150) < y && y < (70 + 0 * 150) && 500 < x && x < 540
    = return $ AppState Settings (subDeltaV exp)
  | (90 - 1 * 150) < y && y < (120 - 1 * 150) && 500 < x && x < 540
    = return $ AppState Settings (addDeltaT exp)
  | (40 - 1 * 150) < y && y < (70 - 1 * 150) && 500 < x && x < 540
    = return $ AppState Settings (subDeltaT exp)
  | otherwise
    = return $ AppState Settings exp
eventAppState (AppState Motorway exp) x y
  | (10 + 1 * 150) < y && y < (150 + 1 * 150)
    = return $ AppState Motorway (addSlowdown exp 1 (x + 1000))
  | (10 + 0 * 150) < y && y < (150 + 0 * 150)
    = return $ AppState Motorway (addSlowdown exp 0 (x + 1000))
  | (10 - 1 * 150) < y && y < (150 - 1 * 150)
    = return $ AppState Motorway (addSlowdown exp (-1) (x + 1000))
  | (10 - 2.5 * 150) < y && y < (150 - 2.5 * 150)
    = return $ AppState Settings exp
  | (10 - 3.5 * 150) < y && y < (150 - 3.5 * 150)
    = exitSuccess
  | otherwise
   = return $ AppState Motorway exp

drawButtoms :: Picture
drawButtoms = Pictures [drawChange, drawExit]

drawSettings :: ((Int, Int) , (Int, Int) , Float , Float) -> Picture
drawSettings ((rangeV1,rangeV2), (rangeT1,rangeT2), deltaV, deltaT) =
  Pictures $
    (map drawName [(2,n1),(1,n2),(0,n3),(-1,n4)]) ++
    (map drawNumeralLeft [2,1,0,-1]) ++
    (map drawBash [2,1]) ++
    (map drawNumeralRidht [2,1]) ++
    [translate (430) (70 + 2 * 150) . scale 0.2 0.2 . color black . text $ show $ 7 * rangeV1
    , translate (640) (70 + 2 * 150) . scale 0.2 0.2 . color black . text $ show $ 7 * rangeV2
    , translate (430) (70 + 1 * 150) . scale 0.2 0.2 . color black . text $ show $ round $ (fromIntegral rangeT1) / 60
    , translate (640) (70 + 1 * 150) . scale 0.2 0.2 . color black . text $ show $ round $ (fromIntegral rangeT2) / 60
    , translate (430) (70 + 0 * 150) . scale 0.2 0.2 . color black . text $ show $ round (8 * deltaV)
    , translate (430) (70 - 1 * 150) . scale 0.2 0.2 . color black . text $ show $ round $ deltaT / 60] ++
    [drawStart, drawExit]
  where
    n1 = "Range of changes in cars speeds (from 10 to 120 km/h)"
    n2 = "Range of an appearance of cars on the road (from 1 to 10 s)"
    n3 = "The amount of reduction in the speed of an artificially braked car (from 10 to 120 km/h)"
    n4 = "The time of movement of an artificially braked car at a lower speed (from 1 to 20 s)"

drawName :: (Float, String) -> Picture
drawName (number,name) = Pictures $
  [Color (dark white) $ polygon
           [(-920, 30 + number * 150),
            (-920, 120 + number * 150),
            (350, 120 + number * 150),
            (350, 30 + number * 150)],
    translate (-900) (70 + number * 150) . scale 0.2 0.2 . color black . text $ name]

drawNumeralLeft :: Float -> Picture
drawNumeralLeft number = Pictures $
  [Color (dark white) $
    polygon [(400, 10 + number * 150),
             (400, 150 + number * 150),
             (550, 150 + number * 150),
             (550, 10 + number * 150)],
      polygon [(520, 40 + number * 150),
               (500, 70 + number * 150),
               (540, 70 + number * 150)],
      polygon [(500, 90 + number * 150),
               (520, 120 + number * 150),
               (540, 90 + number * 150)]]

drawNumeralRidht :: Float -> Picture
drawNumeralRidht number = Pictures $
  [Color (dark white) $
    polygon [(610, 10 + number * 150),
             (610, 150 + number * 150),
             (760, 150 + number * 150),
             (760, 10 + number * 150)],
      polygon [(730, 40 + number * 150),
               (710, 70 + number * 150),
               (750, 70 + number * 150)],
      polygon [(710, 90 + number * 150),
               (730, 120 + number * 150),
               (750, 90 + number * 150)]]

drawBash :: Float -> Picture
drawBash number =
  Color (dark white) $
    polygon [(560, 70 + number * 150),
             (560, 90 + number * 150),
             (600, 90 + number * 150),
             (600, 70 + number * 150)]

drawStart :: Picture
drawStart = Pictures $
  [Color (light $ light red) $
    polygon [(-1000, 10 - 2.5 * 150),
             (-1000, 150 - 2.5 * 150),
             (1000, 150 - 2.5 * 150),
             (1000, 10 - 2.5 * 150)]
    , translate (-150) (-340) . scale 0.8 0.8 . color black . text $ "START"]

drawChange :: Picture
drawChange = Pictures $
  [Color (light $ light red) $
    polygon [(-1000, 10 - 2.5 * 150),
             (-1000, 150 - 2.5 * 150),
             (1000, 150 - 2.5 * 150),
             (1000, 10 - 2.5 * 150)]
    , translate (-900) (-340) . scale 0.8 0.8 . color black . text $ "CHANGE SIMULATION PARAMETERS"]

drawExit :: Picture
drawExit = Pictures $
  [Color (light $ light red) $
    polygon [(-1000, 10 - 3.5 * 150),
             (-1000, 150 - 3.5 * 150),
             (1000, 150 - 3.5 * 150),
             (1000, 10 - 3.5 * 150)]
    , translate (-100) (-490) . scale 0.8 0.8 . color black . text $ "EXIT"]
