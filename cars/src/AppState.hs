module AppState where

import Experiment
import Graphics.Gloss.Interface.IO.Game

data AppState = AppState
  { page :: Page
  , experiment :: Experiment
  }

data Page = Settings | Motorway deriving (Eq)

drawSettings :: Picture
drawSettings =
  polygon [(10,10), (10,-10),(-10,-10), (-10,10)]