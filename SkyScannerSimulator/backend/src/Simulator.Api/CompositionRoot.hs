module CompositionRoot (buildGetAllFlights) where

import Config (Config (..))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Database.HDBC (catchSql)
import Database.HDBC.ODBC (Connection (setAutoCommit), connectODBC)
import FlightAdapter (getAllFlights)
import Ports
import SqlAdapter

getConnection :: String -> IO (Either SqlAdapterError Database.HDBC.ODBC.Connection)
getConnection connectionString = do
  catchSql
    (Right <$> connectODBC connectionString)
    (pure . Left . GeneralError)

setAutoCommitTrue :: Connection -> IO (Either SqlAdapterError Bool)
setAutoCommitTrue connection =
  catchSql
    (Right <$> setAutoCommit connection True)
    (pure . Left . GeneralError)

buildGetAllFlights :: Config -> GetAllFlights SqlAdapterError
buildGetAllFlights config =
  runExceptT
    ( do
        connection <- ExceptT $ getConnection config.sqlConnectionString
        _ <- ExceptT $ setAutoCommitTrue connection
        ExceptT $ getAllFlights connection
    )
