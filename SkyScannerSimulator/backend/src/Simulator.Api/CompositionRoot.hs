module CompositionRoot (buildGetAllFlights, buildTickFlightWorkflow) where

import Config (Config (..))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Database.HDBC (catchSql)
import Database.HDBC.ODBC (Connection (setAutoCommit), connectODBC)
import qualified FlightAdapter (getAllFlights, getFlight, updateFlight)
import Ports
import SqlAdapter
import qualified Workflows.TickFlight (Workflow, execute)

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
        ExceptT $ FlightAdapter.getAllFlights connection
    )

buildTickFlightWorkflow :: Config -> Workflows.TickFlight.Workflow SqlAdapterError
buildTickFlightWorkflow config flightId =
  runExceptT
    ( do
        connection <- ExceptT $ getConnection config.sqlConnectionString
        _ <- ExceptT $ setAutoCommitTrue connection
        let getFlight = FlightAdapter.getFlight connection
        let updateFlight = FlightAdapter.updateFlight connection

        ExceptT $ Workflows.TickFlight.execute getFlight updateFlight flightId
    )
