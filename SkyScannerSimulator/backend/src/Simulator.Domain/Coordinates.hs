module Coordinates (Coordinates (..), calculateIntermediatePoint, calculateDistance, createCoordinateValue, getRawCoordinateValue) where

import qualified GHC.Float as Math
import MathUtils (truncate')

newtype CoordinateValue = CoordinateValue Double deriving (Eq, Show)

data Coordinates = Coordinates {lat :: CoordinateValue, lon :: CoordinateValue} deriving (Eq, Show)

createCoordinateValue :: Double -> CoordinateValue
createCoordinateValue value = do
  let truncated = truncate' value 5
  CoordinateValue truncated

getRawCoordinateValue :: CoordinateValue -> Double
getRawCoordinateValue (CoordinateValue value) = value

degreesToRadians :: Double -> Double
degreesToRadians degrees =
  degrees * pi / 180.0

earthRadiusKm :: Double
earthRadiusKm = 6371.0

calculateDistance :: Coordinates -> Coordinates -> Double
calculateDistance f t = do
  let (CoordinateValue lat2) = lat t
  let (CoordinateValue lat1) = lat f
  let (CoordinateValue lon2) = lon t
  let (CoordinateValue lon1) = lon f

  let dLat = degreesToRadians (lat2 - lat1)
  let dLon = degreesToRadians (lon2 - lon1)

  let dLat1 = degreesToRadians lat1
  let dLat2 = degreesToRadians lat2

  let a = Math.sin (dLat / 2) * Math.sin (dLat / 2) + Math.sin (dLon / 2) * Math.sin (dLon / 2) * Math.cos dLat1 * Math.cos dLat2
  let c = 2 * Math.atan2 (Math.sqrt a) (Math.sqrt (1 - a))
  earthRadiusKm * c

calculateIntermediatePoint :: Coordinates -> Coordinates -> Double -> Coordinates
calculateIntermediatePoint f _ 0.0 = f
calculateIntermediatePoint f t dist = do
  let constant = (pi :: Double) / 180.0
  let angular = dist / earthRadiusKm
  let a = Math.sin ((1 - dist) * angular) / Math.sin angular
  let b = Math.sin (dist * angular) / Math.sin angular

  let (CoordinateValue lat2Value) = lat t
  let (CoordinateValue lat1Value) = lat f
  let (CoordinateValue lon2Value) = lon t
  let (CoordinateValue lon1Value) = lon f

  let lat1 = degreesToRadians lat1Value
  let lat2 = degreesToRadians lat2Value
  let lon1 = degreesToRadians lon1Value
  let lon2 = degreesToRadians lon2Value

  let x =
        a * Math.cos lat1 * Math.cos lon1
          + b * Math.cos lat2 * Math.cos lon2

  let y =
        a * Math.cos lat1 * Math.sin lon1
          + b * Math.cos lat2 * Math.sin lon2

  let z = a * Math.sin lat1 + b * Math.sin lat2

  let lat3 = Math.atan2 z (Math.sqrt (x * x + y * y))
  let lon3 = Math.atan2 y x

  let intermediateLat = lat3 / constant
  let intermediateLon = lon3 / constant
  Coordinates {lat = createCoordinateValue intermediateLat, lon = createCoordinateValue intermediateLon}