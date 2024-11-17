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
  let kilPerTick = kilometersPerTick . plane $ flight
  let fractionPerTick = kilPerTick / calculateDistance (from flight) (to flight)
  let tickFractionProgress = Math.min 1.0 (progress flight + fractionPerTick)
  let intermediatePoint = calculateIntermediatePoint (from flight) (to flight) tickFractionProgress

  let updatedFlight = flight {currentPosition = intermediatePoint, progress = tickFractionProgress}

  case tickFractionProgress of
    1.0 -> endFlight updatedFlight
    _ -> updatedFlight
