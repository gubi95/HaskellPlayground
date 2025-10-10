{-# LANGUAGE OverloadedStrings #-}

module Rooting (rooting) where

import CompositionRoot (buildGetAllFlights, buildTickFlightWorkflow)
import Config (getConfig)
import qualified GetFlights
import Network.Wai.Middleware.Cors
import qualified TickFlight (execute)
import Web.Scotty (captureParam, get, middleware, post, scotty)

rooting :: IO ()
rooting = do
  configResult <- getConfig "/src/Simulator.Api/config.json"
  config <- either fail pure configResult

  scotty 3100 $ do
    middleware simpleCors
    get "/flights" $ GetFlights.execute . buildGetAllFlights $ config
    post "/flights/:id" $ do
      flightId <- captureParam "id"
      TickFlight.execute (buildTickFlightWorkflow config) flightId
