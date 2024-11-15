module Plane (Plane (..), Kind (..)) where

data Plane = Plane {kind :: Kind, distancePerTick :: Double} deriving (Eq, Show)

data Kind = Boeing777 | AirbusA320 | AirbusA380 deriving (Eq, Show)

createKindFromString :: String -> Either String Kind
createKindFromString str = do
    case str of
        "Boeing777" -> Right Boeing777
        "AirbusA320" -> Right AirbusA320
        "AirbusA380" -> Right AirbusA380
        _ -> Left $ "Cannot create Plane Kind from string: " ++ str
