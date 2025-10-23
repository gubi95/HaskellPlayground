module DeferredCleanup (create, cleanupFlight, DeferredCleanup (..), Cleanups (..)) where

import Data.Array.IO (IOArray, MArray (newArray), writeArray)
import Data.IORef
import Ports (DeleteFlight)

data Cleanups
  = Dummy
  | Flight Int
  deriving (Eq, Show)

data DeferredCleanup = DeferredCleanup
  { cleanups :: IOArray Int Cleanups,
    index :: IORef Int,
    deleteFlight :: DeleteFlight String
  }

create :: DeleteFlight String -> IO DeferredCleanup
create deleteFlightPort = do
  array <- newArray (0, 100) Dummy
  actualIndex <- newIORef 0
  return $ DeferredCleanup {cleanups = array, index = actualIndex, deleteFlight = deleteFlightPort}

cleanupFlight :: DeferredCleanup -> Int -> IO ()
cleanupFlight dc flightId = do
  let cleanup = Flight flightId
  actualIndex <- readIORef (index dc)
  writeArray (cleanups dc) actualIndex cleanup
  writeIORef (index dc) (actualIndex + 1)
