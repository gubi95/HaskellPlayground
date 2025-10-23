module Ports (GetAllFlights, GetFlight, CreateFlight, UpdateFlight, DeleteFlight) where

import Flight (Flight)

type GetAllFlights ioError = IO (Either ioError [Flight])

type GetFlight ioError = Int -> IO (Either ioError (Maybe Flight))

type CreateFlight ioError = Flight -> IO (Either ioError Int)

type UpdateFlight ioError = Flight -> IO (Either ioError ())

type DeleteFlight ioError = Int -> IO (Either ioError ())