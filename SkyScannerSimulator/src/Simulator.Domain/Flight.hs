module Flight (Flight (..)) where

import Coordinates
-- import GHC.Float (powerDouble, sqrtDouble)
import Plane

data Flight = Flight
  { plane :: Plane,
    fromLat :: Coordinates,
    fromLon :: Coordinates,
    toLat :: Coordinates,
    toLon :: Coordinates
  }
  deriving (Eq, Show)

-- getRemainingDistance :: Flight -> Double
-- getRemainingDistance flight = do
--   let current = currentPosition flight
--   let destination = to flight

--   let x1 = x current
--   let x2 = x destination

--   let y1 = y current
--   let y2 = y destination

--   sqrtDouble (powerDouble (x1 - x2) 2.0 + powerDouble (y1 - y2) 2.0)

-- tickUntilNewDistanceIsReached :: Double -> (Double -> Double) -> Flight -> Flight
-- tickUntilNewDistanceIsReached desiredDistance calculateNewY flight = do
--   let desitnationX = x . to $ flight
--   let currentX = x . currentPosition $ flight
--   let factor = if desitnationX > currentX then 0.0001 else -0.0001
--   let newCurrentX = currentX + factor
--   let newCurrentY = calculateNewY currentX

--   let updatedFlight = flight {currentPosition = Coordinates {x = newCurrentX, y = newCurrentY}}

--   let currentRemainingDistance = getRemainingDistance updatedFlight

--   let distancePassed = currentRemainingDistance <= desiredDistance

--   if distancePassed then updatedFlight else tickUntilNewDistanceIsReached desiredDistance calculateNewY updatedFlight

-- endFlight :: Flight -> Flight
-- endFlight flight =
--   flight {currentPosition = to flight}

-- tick :: Flight -> Flight
-- tick flight = do
--   let source = from flight
--   let destination = to flight

--   let x1 = x source
--   let x2 = x destination

--   let y1 = y source
--   let y2 = y destination

--   let a = (y2 - y1) / (x2 - x1)
--   let b = y1 - a * x1
--   let calculateNewY newX = a * newX + b

--   let remainingDistance = getRemainingDistance flight
--   let distPerTick = distancePerTick . plane $ flight
--   let distanceToPass = remainingDistance - distPerTick

--   if distanceToPass <= 0.0 then endFlight flight else tickUntilNewDistanceIsReached distanceToPass calculateNewY flight
