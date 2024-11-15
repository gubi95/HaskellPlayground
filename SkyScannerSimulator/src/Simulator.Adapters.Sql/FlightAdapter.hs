{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use mapMaybe" #-}
module FlightAdapter (getAllFlights, FlightDto (..)) where

import Control.Exception (catch)
import Control.Monad.Except
import Data.Either.Extra
import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.ODBC
import SqlAdapter (SqlAdapterError(..))


data FlightDto = FlightDto {from :: String, to :: String, planeModel :: String}
  deriving (Show)

getAllFlights :: Connection -> IO (Either SqlAdapterError [FlightDto])
getAllFlights connection = runExceptT $ do
  sqlValues <-
        ExceptT $
        catch
          ( do
              let query =
                    "\
                    \SELECT f.[From], f.[To], p.[Model] AS [PlaneModel] \
                    \FROM [SimulatorService].[Flights] f \
                    \INNER JOIN [SimulatorService].[Plane] p \
                    \ON f.[PlaneId] = p.[Id]"

              stmt <- prepare connection query

              _ <- execute stmt []

              Right <$> fetchAllRowsMap stmt
          )
          ( pure . Left . GeneralError
          )

  let dtos =
        traverse
          ( \x -> do
              fromSqlValie <- maybeToEither (MissingColumnsInQueryResult "Missing From column") (Map.lookup "From" x)
              toSqlValue <- maybeToEither (MissingColumnsInQueryResult "Missing To column") (Map.lookup "To" x)
              planeModelSqlValue <- maybeToEither (MissingColumnsInQueryResult "Missing PlaneModel column") (Map.lookup "PlaneModel" x)

              let dto =
                    FlightDto
                      { from = fromSql fromSqlValie,
                        to = fromSql toSqlValue,
                        planeModel = fromSql planeModelSqlValue
                      }
              return dto
          )
          sqlValues

  ExceptT (pure dtos)