module Coordinates (Coordinates (..), calculateIntermediatePoint, calculateDistance) where

import qualified GHC.Float as Math

data Coordinates = Coordinates {lat :: Double, lon :: Double} deriving (Eq, Show)

degreesToRadians :: Double -> Double
degreesToRadians degrees =
  degrees * pi / 180.0

earthRadiusKm :: Double
earthRadiusKm = 6371.0

calculateDistance :: Coordinates -> Coordinates -> Double
calculateDistance f t = do
  let lat2 = lat t
  let lat1 = lat f
  let lon2 = lon t
  let lon1 = lon f

  let dLat = degreesToRadians (lat2 - lat1)
  let dLon = degreesToRadians (lon2 - lon1)

  let dLat1 = degreesToRadians lat1
  let dLat2 = degreesToRadians lat2

  let a = Math.sin (dLat / 2) * Math.sin (dLat / 2) + Math.sin (dLon / 2) * Math.sin (dLon / 2) * Math.cos dLat1 * Math.cos dLat2
  let c = 2 * Math.atan2 (Math.sqrt a) (Math.sqrt (1 - a))
  earthRadiusKm * c

calculateIntermediatePoint :: Coordinates -> Coordinates -> Double -> Coordinates
calculateIntermediatePoint f t dist = do
  let constant = (pi :: Double) / 180.0
  let angular = dist / earthRadiusKm
  let a = Math.sin (0 * angular) / Math.sin angular
  let b = Math.sin (1 * angular) / Math.sin angular

  let lat1 = degreesToRadians $ lat f
  let lat2 = degreesToRadians $ lat t
  let lon1 = degreesToRadians $ lon f
  let lon2 = degreesToRadians $ lon t

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
  Coordinates {lat = intermediateLat, lon = intermediateLon}