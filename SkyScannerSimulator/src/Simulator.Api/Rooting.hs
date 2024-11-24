{-# LANGUAGE OverloadedStrings #-}

module Rooting (rooting) where

import Web.Scotty (get, scotty)
import GetFlights
import CompositionRoot (buildGetAllFlights)

rooting :: IO ()
rooting =
  scotty 3000 $
    get "/flights" $ GetFlights.execute buildGetAllFlights
