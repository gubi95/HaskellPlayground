module FlightAdapter (getAllFlights, Dto (..)) where

import Database.HDBC
import Database.HDBC.ODBC

data Dto = Dto {name :: String, id :: Int}

getAllFlights :: () -> IO [[SqlValue]]
getAllFlights () = do
  connection <- connectODBC "DRIVER={ODBC Driver 17 for SQL Server};Server=localhost, 1499;Database=SimulatorService;Uid=sa;Pwd=Secret!Passw0rd;Connection Timeout=30"
  let query = "SELECT [Name] FROM [dbo].[Plane] WHERE [Name] = ?"

  stmt <- prepare connection query

  _ <- execute stmt [toSql ("Airbus" :: String)]

  results <- fetchAllRowsAL stmt

  print (show results)

  return []