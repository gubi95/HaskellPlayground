module CompositionRoot (buildGetAllFlights) where

import Config (Config (..))
import Control.Exception (catch)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Database.HDBC.ODBC (Connection, connectODBC)
import FlightAdapter (getAllFlights)
import Ports
import SqlAdapter

getDbConnection :: String -> IO (Either SqlAdapterError Database.HDBC.ODBC.Connection)
getDbConnection connectionString = do
  catch
    ( do
        connection <- connectODBC connectionString
        pure $ Right connection
    )
    (pure . Left . GeneralError)

buildGetAllFlights :: Config -> GetAllFlights SqlAdapterError
buildGetAllFlights config = runExceptT $ do
  connection <- ExceptT $ getDbConnection config.sqlConnectionString
  ExceptT $ getAllFlights connection