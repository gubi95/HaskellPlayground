module Plane (Plane (..), Kind (..)) where
import StorageCodec (StorageCodec(encode, decode))

data Plane = Plane {kind :: Kind, kilometersPerTick :: Double} deriving (Eq, Show)

data Kind = Boeing777 | AirbusA320 | AirbusA380 deriving (Eq, Show)

instance StorageCodec Kind where
  encode x =
    case x of
      Boeing777 -> "Boeing 777"
      AirbusA320 -> "Airbus A320"
      AirbusA380 -> "Airbus A380"

  decode str =
    case str of
      "Boeing 777" -> Right Boeing777
      "Airbus A320" -> Right AirbusA320
      "Airbus A380" -> Right AirbusA380
      _ -> Left $ "Cannot decode Kind from string: " ++ str
