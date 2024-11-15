module SqlAdapter (SqlAdapterError (..)) where

import Database.HDBC

data SqlAdapterError
  = GeneralError SqlError
  | MissingColumnsInQueryResult String
  deriving (Show)