module Plane (Plane (..), Kind (..), planeKindFromString) where

data Plane = Plane {kind :: Kind, distancePerTick :: Double} deriving (Eq, Show)

data Kind = Boeing777 | AirbusA320 | AirbusA380 deriving (Eq, Show)

planeKindFromString :: String -> Either String Kind
planeKindFromString str = do
    case str of
        "Boeing 777" -> Right Boeing777
        "Airbus A320" -> Right AirbusA320
        "Airbus A380" -> Right AirbusA380
        _ -> Left $ "Cannot create Plane Kind from string: " ++ str
