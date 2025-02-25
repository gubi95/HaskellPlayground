{-# LANGUAGE DeriveGeneric #-}

module Config (Config (..), getConfig) where

import Data.Aeson (eitherDecode)
import Data.Aeson.Types (FromJSON)
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import System.Directory.Extra (getCurrentDirectory)

data Config = Config
  { sqlConnectionString :: String
  }
  deriving (Generic, Show)

instance FromJSON Config

type ConfigFilePath = String

getConfig :: ConfigFilePath -> IO (Either String Config)
getConfig configFilePath = do
  currentDirectory <- getCurrentDirectory
  jsonContent <- readFile (currentDirectory ++ configFilePath)
  return (eitherDecode . fromString $ jsonContent :: Either String Config)