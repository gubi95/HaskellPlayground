{-# LANGUAGE OverloadedStrings #-}

module Rooting (rooting) where

import Web.Scotty (get, scotty)
import GetSchedule

rooting :: IO ()
rooting =
  scotty 3000 $
    get "/schedule" GetSchedule.execute
