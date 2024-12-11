module Ports (GetAllFlights) where

import Flight (Flight)

type GetAllFlights ioError = IO (Either ioError [Flight])