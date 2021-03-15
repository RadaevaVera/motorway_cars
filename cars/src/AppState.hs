module AppState where

import Experiment
import Graphics.Gloss.Interface.IO.Game

data AppState = AppState
  { page :: Page
  , experiment :: Experiment
  }

data Page = Settings | Motorway deriving (Eq)

drawSettings :: (Int, Int) -> (Int, Int) -> Float -> Float -> Picture
drawSettings (rangeV1,rangeV2) (rangeT1,rangeT2) deltaV deltaT =
  Pictures $
    (map drawName [(2,n1),(1,n2),(0,n3),(-1,n4)]) ++
    (map drawNumeralLeft [2,1,0,-1]) ++
    (map drawBash [2,1]) ++
    (map drawNumeralRidht [2,1]) ++
    [translate (430) (70 + 2 * 150) . scale 0.2 0.2 . color black . text $ show rangeV1
    , translate (640) (70 + 2 * 150) . scale 0.2 0.2 . color black . text $ show rangeV2
    , translate (430) (70 + 1 * 150) . scale 0.2 0.2 . color black . text $ show rangeT1
    , translate (640) (70 + 1 * 150) . scale 0.2 0.2 . color black . text $ show rangeT2
    , translate (430) (70 + 0 * 150) . scale 0.2 0.2 . color black . text $ show $ round deltaV
    , translate (430) (70 - 1 * 150) . scale 0.2 0.2 . color black . text $ show $ round deltaT] ++
    [drawStart, drawExit]
  where
    n1 = "Range of changes in cars speeds (from 10 to 120)"
    n2 = "Range of an appearance of cars on the road (from 1 to 10)"
    n3 = "The amount of reduction in the speed of an artificially braked car (from 10 to 120)"
    n4 = "The time of movement of an artificially braked car at a lower speed (from 1 to 20)"

drawName :: (Float, String) -> Picture
drawName (number,name) = Pictures $
  [Color (dark white) $ polygon
           [(-850, 30 + number * 150),
            (-850, 120 + number * 150),
            (350, 120 + number * 150),
            (350, 30 + number * 150)],
    translate (-830) (70 + number * 150) . scale 0.2 0.2 . color black . text $ name]

drawNumeralLeft :: Float -> Picture
drawNumeralLeft number = Pictures $
  [Color (dark white) $
    polygon [(400, 10 + number * 150),
             (400, 150 + number * 150),
             (550, 150 + number * 150),
             (550, 10 + number * 150)],
      polygon [(520, 110 + number * 150),
               (500, 140 + number * 150),
               (540, 140 + number * 150)],
      polygon [(500, 20 + number * 150),
               (520, 50 + number * 150),
               (540, 20 + number * 150)]]

drawNumeralRidht :: Float -> Picture
drawNumeralRidht number = Pictures $
  [Color (dark white) $
    polygon [(610, 10 + number * 150),
             (610, 150 + number * 150),
             (760, 150 + number * 150),
             (760, 10 + number * 150)],
      polygon [(730, 110 + number * 150),
               (710, 140 + number * 150),
               (750, 140 + number * 150)],
      polygon [(710, 20 + number * 150),
               (730, 50 + number * 150),
               (750, 20 + number * 150)]]

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

drawExit :: Picture
drawExit = Pictures $
  [Color (light $ light red) $
    polygon [(-1000, 10 - 3.5 * 150),
             (-1000, 150 - 3.5 * 150),
             (1000, 150 - 3.5 * 150),
             (1000, 10 - 3.5 * 150)]
    , translate (-100) (-490) . scale 0.8 0.8 . color black . text $ "EXIT"]
