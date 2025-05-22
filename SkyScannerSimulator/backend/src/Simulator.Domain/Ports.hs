module Ports (GetAllFlights, GetFlight, UpdateFlight) where

import Flight (Flight)

type GetAllFlights ioError = IO (Either ioError [Flight])

type GetFlight ioError = Int -> IO (Either ioError (Maybe Flight))

type UpdateFlight ioError = Flight -> IO (Either ioError ())