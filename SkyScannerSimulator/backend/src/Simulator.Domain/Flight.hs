module Flight (Flight (..), getRemainingDistance, tick, calculateIntermediatePoint) where

import Coordinates
import qualified GHC.Base as Math
import Plane

data Flight = Flight
  { id :: Int,
    plane :: Plane,
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
  flight {currentPosition = flight.to}

tick :: Flight -> Flight
tick flight = do
  let fractionPerTick = flight.plane.kilometersPerTick / calculateDistance flight.from flight.to
  let tickFractionProgress = Math.min 1.0 (flight.progress + fractionPerTick)
  let intermediatePoint = calculateIntermediatePoint flight.from flight.to tickFractionProgress

  let updatedFlight = flight {currentPosition = intermediatePoint, progress = tickFractionProgress}

  case tickFractionProgress of
    1.0 -> endFlight updatedFlight
    _ -> updatedFlight
