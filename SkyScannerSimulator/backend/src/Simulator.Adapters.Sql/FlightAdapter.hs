module FlightAdapter (getAllFlights) where

import Control.Arrow (ArrowChoice (right), left)
import Control.Exception (catch)
import Control.Monad.Except
import Coordinates
import Database.HDBC
import Database.HDBC.ODBC
import Flight (Flight (..))
import Plane (Plane (..), planeKindFromString)
import Ports (GetAllFlights)
import SqlAdapter (SqlAdapterError (..), getColumnValue)

getAllFlights :: Connection -> GetAllFlights SqlAdapterError
getAllFlights connection = runExceptT $ do
  sqlValues <-
    ExceptT $
      catch
        ( do
            let query =
                  "\
                  \SELECT \
                  \f.[Id],\
                  \p.[Model] AS [PlaneModel],\
                  \p.[KilometersPerTick],\
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

  ExceptT $
    pure $
      traverse
        ( \x -> do
            let parsePlaneModel = left InvalidData . planeKindFromString . fromSql
            flightId <- right fromSql $ getColumnValue "Id" x
            planeKind <- getColumnValue "PlaneModel" x >>= parsePlaneModel
            departureLat <- right fromSql $ getColumnValue "DepartureLat" x
            departureLon <- right fromSql $ getColumnValue "DepartureLon" x
            arrivalLat <- right fromSql $ getColumnValue "ArrivalLat" x
            arrivalLon <- right fromSql $ getColumnValue "ArrivalLon" x
            kmPerTick <- right fromSql $ getColumnValue "KilometersPerTick" x

            pure
              Flight
                { Flight.id = flightId,
                  plane =
                    Plane
                      { kind = planeKind,
                        kilometersPerTick = kmPerTick
                      },
                  from = Coordinates {lat = departureLat, lon = departureLon},
                  to = Coordinates {lat = arrivalLat, lon = arrivalLon},
                  currentPosition = Coordinates {lat = arrivalLat, lon = arrivalLon},
                  progress = 0.0
                }
        )
        sqlValues
