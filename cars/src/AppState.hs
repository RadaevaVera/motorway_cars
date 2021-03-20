module AppState
  ( AppState(..)
  , Page(..)
  , drawButtoms
  , drawSettings)
  where

import Experiment
import Graphics.Gloss.Interface.IO.Game

data AppState = AppState
  { page :: Page
  , experiment :: Experiment
  }

data Page = Settings | Motorway deriving (Eq)

drawButtoms :: Picture
drawButtoms = Pictures [drawChange, drawExit]

drawSettings :: (Int, Int) -> (Int, Int) -> Float -> Float -> Picture
drawSettings (rangeV1,rangeV2) (rangeT1,rangeT2) deltaV deltaT =
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
