module TickFlight (execute) where

import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Network.HTTP.Types (internalServerError500)
import SqlAdapter (SqlAdapterError, describeError)
import Web.Scotty (ActionM, json)
import Web.Scotty.Trans (raiseStatus)
import Workflows.TickFlight (Workflow)

execute :: Workflow SqlAdapterError -> Int -> ActionM ()
execute tickFlightWorkflow flightId = do
  result <- liftIO (tickFlightWorkflow flightId)

  case result of
    Right () -> json ()
    Left e -> raiseStatus internalServerError500 (fromString . describeError $ e)