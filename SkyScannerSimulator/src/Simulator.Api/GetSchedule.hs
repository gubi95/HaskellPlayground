{-# LANGUAGE DeriveGeneric #-}

module GetSchedule (execute) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON)
import GHC.Base (returnIO)
import GHC.Generics (Generic)
import Web.Scotty (ActionM, json)
import Plane

newtype ScheduleDto = ScheduleDto {numberOfFlighths :: Int} deriving (Generic, Show)

instance ToJSON ScheduleDto

execute :: ActionM ()
execute = do
  dto <- liftIO (returnIO $ ScheduleDto {numberOfFlighths = 10})
  json dto