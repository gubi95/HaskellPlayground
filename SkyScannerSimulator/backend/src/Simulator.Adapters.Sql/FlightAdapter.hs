module FlightAdapter (getAllFlights, getFlight, createFlight, updateFlight, deleteFlight) where

import Control.Arrow (ArrowChoice (right), left)
import Control.Monad.Except
import Coordinates
import Data.Map
import Database.HDBC
import Database.HDBC.PostgreSQL (Connection)
import Flight (Airport (..), Flight (..))
import Plane (Plane (..), planeKindFromString, planeKindToString)
import Ports (CreateFlight, DeleteFlight, GetAllFlights, GetFlight, UpdateFlight)
import SqlAdapter (SqlAdapterError (..), getColumnValue)

selectFlightQuery :: String
selectFlightQuery =
  "\
  \SELECT \
  \f.Id,\
  \f.Lat AS CurrentLat,\
  \f.Lon AS CurrentLon,\
  \f.Progress AS Progress,\
  \p.Model AS PlaneModel,\
  \p.KilometersPerTick,\
  \departureAirport.Lat AS DepartureLat,\
  \departureAirport.Lon AS DepartureLon,\
  \departureAirport.Code AS DepartureAirportCode,\
  \arrivalAirport.Lat AS ArrivalLat,\
  \arrivalAirport.Lon AS ArrivalLon,\
  \arrivalAirport.Code AS ArrivalAirportCode \
  \FROM SimulatorService.Flights f \
  \INNER JOIN SimulatorService.Plane p \
  \ON f.PlaneId = p.Id \
  \INNER JOIN SimulatorService.Airport departureAirport \
  \ON f.DepartureAirportId = departureAirport.Id \
  \INNER JOIN SimulatorService.Airport arrivalAirport \
  \ON f.ArrivalAirportId = arrivalAirport.Id \
  \"

parseFlightDto :: Map String SqlValue -> Either SqlAdapterError Flight
parseFlightDto x = do
  let parsePlaneModel = left InvalidData . planeKindFromString . fromSql
  flightId <- right fromSql $ getColumnValue "Id" x
  currentLat <- right (createCoordinateValue . fromSql) $ getColumnValue "CurrentLat" x
  currentLon <- right (createCoordinateValue . fromSql) $ getColumnValue "CurrentLon" x
  currentProgress <- right fromSql $ getColumnValue "Progress" x
  planeKind <- getColumnValue "PlaneModel" x >>= parsePlaneModel
  departureLat <- right (createCoordinateValue . fromSql) $ getColumnValue "DepartureLat" x
  departureLon <- right (createCoordinateValue . fromSql) $ getColumnValue "DepartureLon" x
  departureAirportCode <- right fromSql $ getColumnValue "DepartureAirportCode" x
  arrivalLat <- right (createCoordinateValue . fromSql) $ getColumnValue "ArrivalLat" x
  arrivalLon <- right (createCoordinateValue . fromSql) $ getColumnValue "ArrivalLon" x
  arrivalAirportCode <- right fromSql $ getColumnValue "ArrivalAirportCode" x
  kmPerTick <- right fromSql $ getColumnValue "KilometersPerTick" x
  pure
    Flight
      { id = flightId,
        plane =
          Plane
            { kind = planeKind,
              kilometersPerTick = kmPerTick
            },
        from = Airport {location = Coordinates {lat = departureLat, lon = departureLon}, code = departureAirportCode},
        to = Airport {location = Coordinates {lat = arrivalLat, lon = arrivalLon}, code = arrivalAirportCode},
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
            let query = selectFlightQuery ++ " WHERE f.id = ?"
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
            let query = "UPDATE SimulatorService.Flights SET lat = ?, lon = ?, progress = ? WHERE id = ?"

            stmt <- prepare connection query

            _ <-
              execute
                stmt
                [ toSql . getRawCoordinateValue $ flight.currentPosition.lat,
                  toSql . getRawCoordinateValue $ flight.currentPosition.lon,
                  toSql flight.progress,
                  toSql flight.id
                ]

            _ <- commit connection

            return $ Right ()
        )
        ( pure . Left . GeneralError
        )

createFlight :: Connection -> CreateFlight SqlAdapterError
createFlight connection flight =
  runExceptT $ do
    ExceptT $
      catchSql
        ( do
            let query =
                  "\
                  \INSERT INTO SimulatorService.Flights (planeid, departureairportid, arrivalairportid, lat, lon, progress) \
                  \VALUES (\
                  \(SELECT id FROM SimulatorService.Plane WHERE model = ?),\
                  \(SELECT id FROM SimulatorService.Airport WHERE code = ?),\
                  \(SELECT id FROM SimulatorService.Airport WHERE code = ?),\
                  \?,\
                  \?,\
                  \?) RETURNING id;"

            stmt <- prepare connection query

            _ <-
              execute
                stmt
                [ toSql (planeKindToString flight.plane.kind),
                  toSql flight.from.code,
                  toSql flight.to.code,
                  toSql . getRawCoordinateValue $ flight.currentPosition.lat,
                  toSql . getRawCoordinateValue $ flight.currentPosition.lon,
                  toSql flight.progress
                ]

            flightId <- fetchRow stmt

            _ <- commit connection

            return $
              case flightId of
                Just [sqlId] -> Right (fromSql sqlId :: Int)
                _ -> Left . MissingInsertedRecordId $ "Failed to retrieve inserted flight ID"
        )
        ( pure . Left . GeneralError
        )

deleteFlight :: Connection -> DeleteFlight SqlAdapterError
deleteFlight connection flightId =
  runExceptT $ do
    ExceptT $
      catchSql
        ( do
            let query = "DELETE FROM SimulatorService.Flights WHERE id = ?"

            stmt <- prepare connection query

            _ <- execute stmt [toSql flightId]

            _ <- commit connection

            return $ Right ()
        )
        ( pure . Left . GeneralError
        )
