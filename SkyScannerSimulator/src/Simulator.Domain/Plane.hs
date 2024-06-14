module Plane (Plane (..), PlaneType (..)) where

data Plane = Plane {kind :: PlaneType, distancePerTick :: Double} deriving (Eq, Show)

data PlaneType = Boeing | Airbus deriving (Eq, Show)