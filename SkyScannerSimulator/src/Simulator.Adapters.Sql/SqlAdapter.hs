module SqlAdapter (SqlAdapterError (..), getColumnValue) where

import Data.Either.Extra (maybeToEither)
import Data.Map (Map, lookup)
import Database.HDBC

data SqlAdapterError
  = GeneralError SqlError
  | MissingColumnsInQueryResult String
  | InvalidData String
  deriving (Show)

getColumnValue :: String -> Map String SqlValue -> Either SqlAdapterError SqlValue
getColumnValue columnName x =
  maybeToEither (MissingColumnsInQueryResult $ "Missing column: " ++ columnName) (Data.Map.lookup columnName x)