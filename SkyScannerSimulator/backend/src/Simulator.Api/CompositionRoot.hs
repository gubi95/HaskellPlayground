module CompositionRoot (buildGetAllFlights) where

import Control.Exception (catch)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Database.HDBC.ODBC (Connection, connectODBC)
import FlightAdapter (getAllFlights)
import Ports
import SqlAdapter

getDbConnection :: () -> IO (Either SqlAdapterError Database.HDBC.ODBC.Connection)
getDbConnection () = do
  catch
    ( do
        connection <- connectODBC "DRIVER={ODBC Driver 17 for SQL Server};Server=localhost, 1499;Database=SimulatorService;Uid=sa;Pwd=Secret!Passw0rd;Connection Timeout=30"
        pure $ Right connection
    )
    (pure . Left . GeneralError)

buildGetAllFlights :: GetAllFlights SqlAdapterError
buildGetAllFlights = runExceptT $ do
  connection <- ExceptT $ getDbConnection ()
  ExceptT $ getAllFlights connection