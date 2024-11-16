{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Coordinates (Coordinates (..), fromString) where

import Data.Either.Extra (maybeToEither)
import Data.List.Extra (splitOn)
import Text.Read (readMaybe)

data Coordinates = Coordinates {degree :: Int, minutes :: Int, seconds :: Int} deriving (Eq, Show)

fromString :: String -> Either String Coordinates
fromString str =
  case splitOn "_" str of
    [degree, minutes, seconds] -> do
      d <- maybeToEither "Cannot convert degree to Int" (readMaybe degree :: Maybe Int)
      m <- maybeToEither "Cannot convert minutes to Int" (readMaybe minutes :: Maybe Int)
      s <- maybeToEither "Cannot convert seconds to Int" (readMaybe seconds :: Maybe Int)
      let coordinates = Coordinates {degree = d, minutes = m, seconds = s}
      pure coordinates
    _ -> Left $ "Cannot create Coordinates from string: " ++ str