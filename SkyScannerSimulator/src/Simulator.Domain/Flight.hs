module Flight (Flight (..), getRemainingDistance, tick) where

import Coordinates
import qualified GHC.Float as Math
import Plane

data Flight = Flight
  { plane :: Plane,
    from :: Coordinates,
    to :: Coordinates,
    currentPosition :: Coordinates
  }
  deriving (Eq, Show)

degreesToRadians :: Double -> Double
degreesToRadians degrees =
  degrees * pi / 180.0

getRemainingDistance :: Flight -> Double
getRemainingDistance flight = do
  let earthRadiusKm = 6371.0

  let lat2 = lat . to $ flight
  let lat1 = lat . from $ flight
  let lon2 = lon . to $ flight
  let lon1 = lon . from $ flight

  let dLat = degreesToRadians (lat2 - lat1)
  let dLon = degreesToRadians (lon2 - lon1)

  let dLat1 = degreesToRadians lat1
  let dLat2 = degreesToRadians lat2

  let a = Math.sin (dLat / 2) * Math.sin (dLat / 2) + Math.sin (dLon / 2) * Math.sin (dLon / 2) * Math.cos dLat1 * Math.cos dLat2
  let c = 2 * Math.atan2 (Math.sqrt a) (Math.sqrt (1 - a))
  earthRadiusKm * c

tickUntilNewDistanceIsReached :: Double -> (Double -> Double) -> Flight -> Flight
tickUntilNewDistanceIsReached desiredDistance calculateNewY flight = do
  let desitnationX = lat . to $ flight
  let currentX = lon . currentPosition $ flight
  let factor = if desitnationX > currentX then 0.0001 else -0.0001
  let newCurrentX = currentX + factor
  let newCurrentY = calculateNewY currentX

  let updatedFlight = flight {currentPosition = Coordinates {lat = newCurrentX, lon = newCurrentY}}

  let currentRemainingDistance = getRemainingDistance updatedFlight

  let distancePassed = currentRemainingDistance <= desiredDistance

  if distancePassed then updatedFlight else tickUntilNewDistanceIsReached desiredDistance calculateNewY updatedFlight

endFlight :: Flight -> Flight
endFlight flight =
  flight {currentPosition = to flight}

tick :: Flight -> Flight
tick flight = do
  let source = from flight
  let destination = to flight

  let x1 = lat source
  let x2 = lat destination

  let y1 = lon source
  let y2 = lon destination

  let a = (y2 - y1) / (x2 - x1)
  let b = y1 - a * x1
  let calculateNewY newX = a * newX + b

  let remainingDistance = getRemainingDistance flight
  let distPerTick = distancePerTick . plane $ flight
  let distanceToPass = remainingDistance - distPerTick

  if distanceToPass <= 0.0 then endFlight flight else tickUntilNewDistanceIsReached distanceToPass calculateNewY flight
