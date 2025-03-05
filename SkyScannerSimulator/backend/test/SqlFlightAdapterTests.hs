module SqlFlightAdapterTests (tests) where

import Coordinates
import Database.HDBC.ODBC (Connection (setAutoCommit), connectODBC)
import Flight
import FlightAdapter (getAllFlights, updateFlight)
import Plane
import SqlAdapter (describeError)
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual)

shouldReturnAllFlightsTest :: String -> Test
shouldReturnAllFlightsTest connectionString =
  TestCase $
    do
      connection <- connectODBC connectionString
      actualFlights <- getAllFlights connection

      let expectedFlights =
            [ Flight
                { Flight.id = 1,
                  plane = Plane {kind = Boeing777, kilometersPerTick = 10.0},
                  from = Coordinates {lat = 33.94032669067383, lon = -118.4123306274414},
                  to = Coordinates {lat = 40.646148681640625, lon = -73.78596496582031},
                  currentPosition = Coordinates {lat = 33.94032669067383, lon = -118.4123306274414},
                  progress = 0.0
                }
            ]

      assertEqual "Should return flights" (Right expectedFlights) actualFlights

shouldUpdateFlightTest :: String -> Test
shouldUpdateFlightTest connectionString =
  TestCase $
    do
      connection1 <- connectODBC connectionString
      _ <- setAutoCommit connection1 True
      actualFlights <- either (error . describeError) Prelude.id <$> getAllFlights connection1

      let flight = head actualFlights

      let updatedFlight = flight {progress = 50}

      connection2 <- connectODBC connectionString
      _ <- setAutoCommit connection2 True
      _ <- either (error . describeError) Prelude.id <$> updateFlight connection2 updatedFlight

      connection3 <- connectODBC connectionString
      _ <- setAutoCommit connection3 True
      actualUpdatedFlights <- either (fail . describeError) Prelude.id <$> getAllFlights connection3

      let actualUpdatedFlight = head actualUpdatedFlights

      assertEqual "Should return updated flight" updatedFlight actualUpdatedFlight

tests :: String -> Test
tests connectionString =
  TestList
    [ TestLabel "Should return all flights" $ shouldReturnAllFlightsTest connectionString,
      TestLabel "Should update flight" $ shouldUpdateFlightTest connectionString
    ]