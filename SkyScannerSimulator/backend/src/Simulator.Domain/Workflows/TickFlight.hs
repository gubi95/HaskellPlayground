module Workflows.TickFlight (execute, Workflow) where

import Flight (tick)
import Ports (GetFlight, UpdateFlight)

type Workflow ioError = Int -> IO (Either ioError ())

execute :: GetFlight ioError -> UpdateFlight ioError -> Workflow ioError
execute getFlight updateFlight flightId = do
  getFlightResult <- getFlight flightId

  case getFlightResult of
    Left e -> pure $ Left e
    Right (Just flight) -> do
      updateFlight $ tick flight
    Right Nothing -> pure $ Right ()