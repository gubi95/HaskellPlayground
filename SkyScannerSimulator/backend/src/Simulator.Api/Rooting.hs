{-# LANGUAGE OverloadedStrings #-}

module Rooting (rooting) where

import Web.Scotty (get, scotty, middleware)
import GetFlights
import CompositionRoot (buildGetAllFlights)
import Network.Wai.Middleware.Cors

rooting :: IO ()
rooting =
  scotty 3000 $ do
    middleware simpleCors
    get "/flights" $ GetFlights.execute buildGetAllFlights
