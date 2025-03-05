{-# LANGUAGE OverloadedRecordDot #-}

import Config (Config (..), getConfig)
import DomainFlightTests
import SqlFlightAdapterTests
import Test.HUnit (Counts, Test (TestLabel, TestList), runTestTT)

main :: IO Counts
main = do
  testConfig <- either error id <$> getConfig "/test/config.json"

  runTestTT
    ( TestList
        [ TestLabel "Domain.Flight tests" DomainFlightTests.tests,
          TestLabel "Sql.FlightAdapter tests" $ SqlFlightAdapterTests.tests testConfig.sqlConnectionString
        ]
    )
