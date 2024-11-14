{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use mapMaybe" #-}
module FlightAdapter (getAllFlights, FlightDto (..)) where

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Database.HDBC
import Database.HDBC.ODBC

data FlightDto = FlightDto {from :: String, to :: String, planeModel :: String}
  deriving (Show)

getAllFlights :: Connection -> IO [FlightDto]
getAllFlights connection = do
  let query =
        "\
        \SELECT f.[From], f.[To], p.[Model] AS [PlaneModel] \
        \FROM [SimulatorService].[Flights] f \
        \INNER JOIN [SimulatorService].[Plane] p \
        \ON f.[PlaneId] = p.[Id] \
        \WHERE f.[To] = ?"

  stmt <- prepare connection query

  _ <- execute stmt [toSql ("Los Angeles" :: String)]

  results <- fetchAllRowsMap stmt

  let dtos =
        catMaybes
          . map
            ( \x -> do
                fromSqlValie <- Map.lookup "From" x
                toSqlValue <- Map.lookup "To" x
                planeModelSqlValue <- Map.lookup "PlaneModel" x

                let dto =
                      FlightDto
                        { from = fromSql fromSqlValie,
                          to = fromSql toSqlValue,
                          planeModel = fromSql planeModelSqlValue
                        }
                return dto
            )
          $ results

  return dtos