{-# LANGUAGE OverloadedRecordDot #-}

import Config (Config (..), getConfig)
import Control.Monad (forM_, (>=>))
import Data.Array.IO (getElems)
import DeferredCleanup (Cleanups (Dummy, Flight))
import qualified DeferredCleanup
import DomainFlightTests
import FlightAdapter (deleteFlight)
import SqlFlightAdapterTests
import Test.HUnit (Counts, Test (TestCase, TestLabel, TestList), runTestTT)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import SqlAdapter (describeError)
import Control.Arrow (left)

main :: IO Counts
main = do
  testConfig <- either error id <$> getConfig "/test/config.json"
  cleanupConnection <- connectPostgreSQL testConfig.sqlConnectionString
  let deleteFlightPort = (deleteFlight cleanupConnection) >=> (return . left describeError)
  deferredCleanup <- DeferredCleanup.create deleteFlightPort

  runTestTT
    ( TestList
        [ TestLabel "Domain.Flight tests" DomainFlightTests.tests,
          TestLabel "Sql.FlightAdapter tests" $ SqlFlightAdapterTests.tests testConfig.sqlConnectionString deferredCleanup,
          TestLabel "Cleanup" $ TestCase $ do
            allCleanups <- getElems deferredCleanup.cleanups

            forM_ allCleanups $ \cleanup -> do
              case cleanup of
                Flight flightId -> either error id <$> deferredCleanup.deleteFlight flightId
                Dummy -> pure ()
        ]
    )
