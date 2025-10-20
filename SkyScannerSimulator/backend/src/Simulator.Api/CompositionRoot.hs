module CompositionRoot (buildGetAllFlights, buildTickFlightWorkflow) where

import Config (Config (..))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Database.HDBC (catchSql)
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import qualified FlightAdapter (getAllFlights, getFlight, updateFlight)
import Ports
import SqlAdapter
import qualified Workflows.TickFlight (Workflow, execute)

getConnection :: String -> IO (Either SqlAdapterError Connection)
getConnection connectionString = do
  catchSql
    (Right <$> connectPostgreSQL connectionString)
    (pure . Left . GeneralError)

buildGetAllFlights :: Config -> GetAllFlights SqlAdapterError
buildGetAllFlights config =
  runExceptT
    ( do
        connection <- ExceptT $ getConnection config.sqlConnectionString
        ExceptT $ FlightAdapter.getAllFlights connection
    )

buildTickFlightWorkflow :: Config -> Workflows.TickFlight.Workflow SqlAdapterError
buildTickFlightWorkflow config flightId =
  runExceptT
    ( do
        connection <- ExceptT $ getConnection config.sqlConnectionString
        let getFlight = FlightAdapter.getFlight connection
        let updateFlight = FlightAdapter.updateFlight connection

        ExceptT $ Workflows.TickFlight.execute getFlight updateFlight flightId
    )
