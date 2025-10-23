{-# LANGUAGE DeriveGeneric #-}

module GetFlights (execute) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Coordinates (Coordinates (lat), lon, getRawCoordinateValue)
import Data.Aeson (ToJSON)
import Flight
import GHC.Generics (Generic)
import Network.HTTP.Types (internalServerError500)
import Plane (kind, planeKindToString)
import Ports (GetAllFlights)
import SqlAdapter (SqlAdapterError, describeError)
import Web.Scotty (ActionM, json, raiseStatus)
import Data.String (IsString(fromString))

data FlightDto = FlightDto
  { id :: Int,
    plane :: String,
    fromLat :: Double,
    fromLon :: Double,
    toLat :: Double,
    toLon :: Double,
    currentLat :: Double,
    currentLon :: Double
  }
  deriving (Generic, Show)

flightToDto :: Flight -> FlightDto
flightToDto flight =
  FlightDto
    { id = flight.id,
      plane = planeKindToString flight.plane.kind,
      fromLat = getRawCoordinateValue flight.from.location.lat,
      fromLon = getRawCoordinateValue flight.from.location.lon,
      toLat = getRawCoordinateValue flight.to.location.lat,
      toLon = getRawCoordinateValue flight.to.location.lon,
      currentLat = getRawCoordinateValue flight.currentPosition.lat,
      currentLon = getRawCoordinateValue flight.currentPosition.lon
    }

instance ToJSON FlightDto

execute :: GetAllFlights SqlAdapterError -> ActionM ()
execute buildGetFlights = do
  result <- liftIO buildGetFlights

  case result of
    Right flights -> json $ flightToDto <$> flights
    Left e -> raiseStatus internalServerError500 (fromString . describeError $ e)