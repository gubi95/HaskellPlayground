module Flight (Flight (..), getRemainingDistance, tick, calculateIntermediatePoint) where

import Coordinates
import qualified GHC.Base as Math
import Plane

data Flight = Flight
  { plane :: Plane,
    from :: Coordinates,
    to :: Coordinates,
    currentPosition :: Coordinates,
    progress :: Double
  }
  deriving (Eq, Show)

getRemainingDistance :: Flight -> Double
getRemainingDistance flight = do
  calculateDistance (currentPosition flight) (to flight)

endFlight :: Flight -> Flight
endFlight flight =
  flight {currentPosition = to flight}

tick :: Flight -> Flight
tick flight = do
  let distPerTick = distancePerTick . plane $ flight
  let tickProgress = Math.min 1.0 (progress flight + distPerTick)
  let intermediatePoint = calculateIntermediatePoint (from flight) (to flight) tickProgress

  let updatedFlight = flight {currentPosition = intermediatePoint, progress = tickProgress}

  case tickProgress of
    1.0 -> endFlight updatedFlight
    _ -> updatedFlight
