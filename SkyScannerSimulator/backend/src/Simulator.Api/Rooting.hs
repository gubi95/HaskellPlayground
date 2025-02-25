{-# LANGUAGE OverloadedStrings #-}

module Rooting (rooting) where

import CompositionRoot (buildGetAllFlights)
import Config (getConfig)
import GetFlights
import Network.Wai.Middleware.Cors
import Web.Scotty (get, middleware, scotty)

rooting :: IO ()
rooting = do
  configResult <- getConfig "/src/Simulator.Api/config.json"
  config <- either fail pure configResult

  scotty 3000 $ do
    middleware simpleCors
    get "/flights" $ GetFlights.execute . buildGetAllFlights $ config
