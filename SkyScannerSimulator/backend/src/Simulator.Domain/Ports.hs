module Ports (GetAllFlights, UpdateFlight) where

import Flight (Flight)

type GetAllFlights ioError = IO (Either ioError [Flight])

type UpdateFlight ioError = Flight -> IO (Either ioError ())