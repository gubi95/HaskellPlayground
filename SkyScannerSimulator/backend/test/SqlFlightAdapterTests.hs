module SqlFlightAdapterTests (tests) where

import Coordinates
import Database.HDBC.ODBC (connectODBC)
import Flight
import FlightAdapter (getAllFlights)
import Plane
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual)

shouldReturnAllFlightsTest :: Test
shouldReturnAllFlightsTest =
  TestCase $
    do
      connection <- connectODBC "DRIVER={ODBC Driver 17 for SQL Server};Server=localhost, 1499;Database=SimulatorService;Uid=sa;Pwd=Secret!Passw0rd;Connection Timeout=30"
      actualFlights <- getAllFlights connection

      let expectedFlights =
            [ Flight
                { Flight.id = 1,
                  plane = Plane {kind = Boeing777, kilometersPerTick = 10.0},
                  from = Coordinates {lat = 33.94032669067383, lon = -118.4123306274414},
                  to = Coordinates {lat = 40.646148681640625, lon = -73.78596496582031},
                  currentPosition = Coordinates {lat = 40.646148681640625, lon = -73.78596496582031},
                  progress = 0.0
                }
            ]

      assertEqual "Should return flights" (Right expectedFlights) actualFlights

tests :: Test
tests =
  TestList
    [ TestLabel "Should return all flights" shouldReturnAllFlightsTest
    ]