module Flight (Flight (..), getRemainingDistance, tick, calculateIntermediatePoint) where

import Coordinates
import Plane

data Flight = Flight
  { plane :: Plane,
    from :: Coordinates,
    to :: Coordinates,
    currentPosition :: Coordinates
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
  let actualRemainingDistance = getRemainingDistance flight
  let intermediatePoint = calculateIntermediatePoint (from flight) (to flight) distPerTick

  let updatedFlight = flight {currentPosition = intermediatePoint}

  let newDistanceRemaining = getRemainingDistance updatedFlight

  if newDistanceRemaining < distPerTick
    then
      updatedFlight
    else
      if actualRemainingDistance - newDistanceRemaining > distPerTick
        then
          tick updatedFlight
        else endFlight updatedFlight
