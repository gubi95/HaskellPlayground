module FlightAdapter (getAllFlights, getFlight, updateFlight) where

import Control.Arrow (ArrowChoice (right), left)
import Control.Monad.Except
import Coordinates
import Data.Map
import Database.HDBC
import Database.HDBC.ODBC
import Flight (Flight (..))
import Plane (Plane (..), planeKindFromString)
import Ports (GetAllFlights, GetFlight, UpdateFlight)
import SqlAdapter (SqlAdapterError (..), getColumnValue)

selectFlightQuery :: String
selectFlightQuery =
  "\
  \SELECT \
  \f.[Id],\
  \f.[Lat] AS [CurrentLat],\
  \f.[Lon] AS [CurrentLon],\
  \f.[Progress] AS [Progress],\
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

parseFlightDto :: Map String SqlValue -> Either SqlAdapterError Flight
parseFlightDto x = do
  let parsePlaneModel = left InvalidData . planeKindFromString . fromSql
  flightId <- right fromSql $ getColumnValue "Id" x
  currentLat <- right fromSql $ getColumnValue "CurrentLat" x
  currentLon <- right fromSql $ getColumnValue "CurrentLon" x
  currentProgress <- right fromSql $ getColumnValue "Progress" x
  planeKind <- getColumnValue "PlaneModel" x >>= parsePlaneModel
  departureLat <- right fromSql $ getColumnValue "DepartureLat" x
  departureLon <- right fromSql $ getColumnValue "DepartureLon" x
  arrivalLat <- right fromSql $ getColumnValue "ArrivalLat" x
  arrivalLon <- right fromSql $ getColumnValue "ArrivalLon" x
  kmPerTick <- right fromSql $ getColumnValue "KilometersPerTick" x
  pure
    Flight
      { id = flightId,
        plane =
          Plane
            { kind = planeKind,
              kilometersPerTick = kmPerTick
            },
        from = Coordinates {lat = departureLat, lon = departureLon},
        to = Coordinates {lat = arrivalLat, lon = arrivalLon},
        currentPosition = Coordinates {lat = currentLat, lon = currentLon},
        progress = currentProgress
      }

getAllFlights :: Connection -> GetAllFlights SqlAdapterError
getAllFlights connection = runExceptT $ do
  sqlValues <-
    ExceptT $
      catchSql
        ( do
            stmt <- prepare connection selectFlightQuery

            _ <- execute stmt []

            Right <$> fetchAllRowsMap stmt
        )
        ( pure . Left . GeneralError
        )

  ExceptT $
    pure $
      traverse
        parseFlightDto
        sqlValues

getFlight :: Connection -> GetFlight SqlAdapterError
getFlight connection flightId = runExceptT $ do
  sqlValues <-
    ExceptT $
      catchSql
        ( do
            let query = selectFlightQuery ++ " WHERE f.[Id] = ?"
            stmt <- prepare connection query

            _ <- execute stmt [toSql flightId]

            Right <$> fetchRowMap stmt
        )
        ( pure . Left . GeneralError
        )

  ExceptT $
    pure $
      maybe (Right Nothing) (fmap Just . parseFlightDto) sqlValues

updateFlight :: Connection -> UpdateFlight SqlAdapterError
updateFlight connection flight =
  runExceptT $ do
    ExceptT $
      catchSql
        ( do
            let query = "UPDATE [SimulatorService].[Flights] SET [Lat] = ?, [Lon] = ?, [Progress] = ? WHERE [Id] = ?"

            stmt <- prepare connection query

            _ <-
              execute
                stmt
                [ toSql flight.currentPosition.lat,
                  toSql flight.currentPosition.lon,
                  toSql flight.progress,
                  toSql flight.id
                ]

            return $ Right ()
        )
        ( pure . Left . GeneralError
        )
