module Flight (Flight (..), Airport (..), getRemainingDistance, tick, calculateIntermediatePoint) where

import Coordinates
import qualified GHC.Base as Math
import Plane

data Airport = Airport {
  code :: String,
  location :: Coordinates
} deriving (Eq, Show)

data Flight = Flight
  { id :: Int,
    plane :: Plane,
    from :: Airport,
    to :: Airport,
    currentPosition :: Coordinates,
    progress :: Double
  }
  deriving (Eq, Show)

getRemainingDistance :: Flight -> Int
getRemainingDistance flight = do
  round (calculateDistance flight.currentPosition flight.to.location)

endFlight :: Flight -> Flight
endFlight flight =
  flight {currentPosition = flight.to.location}

tick :: Flight -> Flight
tick flight = do
  let fractionPerTick = flight.plane.kilometersPerTick / calculateDistance flight.from.location flight.to.location
  let tickFractionProgress = Math.min 1.0 (flight.progress + fractionPerTick)
  let intermediatePoint = calculateIntermediatePoint flight.from.location flight.to.location tickFractionProgress

  let updatedFlight = flight {currentPosition = intermediatePoint, progress = tickFractionProgress}

  case tickFractionProgress of
    1.0 -> endFlight updatedFlight
    _ -> updatedFlight
