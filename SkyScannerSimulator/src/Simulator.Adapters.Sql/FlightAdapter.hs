{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use mapMaybe" #-}
module FlightAdapter (getAllFlights, FlightDto (..)) where

import Control.Arrow (left)
import Control.Exception (catch)
import Control.Monad.Except
import Coordinates (fromString)
import Database.HDBC
import Database.HDBC.ODBC
import Flight (Flight (..))
import Plane (Plane (..), planeKindFromString)
import SqlAdapter (SqlAdapterError (..), getColumnValue)

data FlightDto = FlightDto
  { planeModel :: String,
    departureLat :: String,
    departureLon :: String,
    arrivalLat :: String,
    arrivalLon :: String
  }
  deriving (Show)

getAllFlights :: Connection -> IO (Either SqlAdapterError [Flight])
getAllFlights connection = runExceptT $ do
  sqlValues <-
    ExceptT $
      catch
        ( do
            let query =
                  "\
                  \SELECT \
                  \p.[Model] AS [PlaneModel],\
                  \departureAirport.[Lat] AS [DepartureLat],\
                  \departureAirport.[Lon] AS [DepartureLon],\
                  \arrivalAirport.[Lat] AS [ArrivalLat],\
                  \arrivalAirport.[Lon] AS [ArrivalLon] \
                  \FROM [SimulatorService].[Flights] f \
                  \INNER JOIN [SimulatorService].[Plane] p \
                  \ON f.[PlaneId] = p.[Id]\
                  \INNER JOIN [SimulatorService].[Airport] departureAirport \
                  \ON f.[DepartureAirportId] = departureAirport.[Id] \
                  \INNER JOIN [SimulatorService].[Airport] arrivalAirport \
                  \ON f.[ArrivalAirportId] = arrivalAirport.[Id]\
                  \"

            stmt <- prepare connection query

            _ <- execute stmt []

            Right <$> fetchAllRowsMap stmt
        )
        ( pure . Left . GeneralError
        )

  dtos <-
    ExceptT $
      pure $
        traverse
          ( \x -> do
              planeModelSqlValue <- getColumnValue "PlaneModel" x
              departureLatSqlValue <- getColumnValue "DepartureLat" x
              departureLonSqlValue <- getColumnValue "DepartureLon" x
              arrivalLatSqlValue <- getColumnValue "ArrivalLat" x
              arrivalLonSqlValue <- getColumnValue "ArrivalLon" x

              pure
                FlightDto
                  { planeModel = fromSql planeModelSqlValue,
                    departureLat = fromSql departureLatSqlValue,
                    departureLon = fromSql departureLonSqlValue,
                    arrivalLat = fromSql arrivalLatSqlValue,
                    arrivalLon = fromSql arrivalLonSqlValue
                  }
          )
          sqlValues

  ExceptT . pure $
    traverse
      ( \dto -> do
          planeKind <- left InvalidData $ planeKindFromString $ planeModel dto
          fLat <- left InvalidData $ fromString $ departureLat dto
          fLon <- left InvalidData $ fromString $ departureLon dto
          tLat <- left InvalidData $ fromString $ arrivalLat dto
          tLon <- left InvalidData $ fromString $ arrivalLon dto

          pure
            Flight
              { plane =
                  Plane
                    { kind = planeKind,
                      distancePerTick = 0.0
                    },
                fromLat = fLat,
                fromLon = fLon,
                toLat = tLat,
                toLon = tLon
              }
      )
      dtos