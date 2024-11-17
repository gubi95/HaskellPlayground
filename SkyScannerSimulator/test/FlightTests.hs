module FlightTests (tests) where

import Coordinates (Coordinates (..))
import Database.HDBC.ODBC
import Flight (Flight (..), getRemainingDistance, tick)
import FlightAdapter
import Plane
import Test.HUnit

shouldCalculateRemainingFlightDistance :: Test
shouldCalculateRemainingFlightDistance =
  TestCase
    ( do
        let flight =
              Flight
                { plane = Plane {kind = AirbusA380, distancePerTick = 0},
                  from = Coordinates {lat = 51.5, lon = 0.0},
                  to = Coordinates {lat = 38.8, lon = -77.1},
                  currentPosition = Coordinates {lat = 51.5, lon = 0.0}
                }

        let actualRemainingDistance = getRemainingDistance flight

        assertEqual "Should calculare remaining distance correctly" 5918.185064088764 actualRemainingDistance
    )

flightShouldReachItsDestinationByTicking :: Test
flightShouldReachItsDestinationByTicking =
  TestCase
    ( do
        let plane = Plane {kind = AirbusA380, distancePerTick = 1}
        let fromDestination = Coordinates {lat = 0.0, lon = 0.0}
        let toDestination = Coordinates {lat = 4.0, lon = 5.0}
        let flight = Flight {plane = plane, Flight.from = fromDestination, Flight.to = toDestination, currentPosition = fromDestination}

        let updatedFlight1 = tick flight
        let updatedFlight2 = tick updatedFlight1
        let updatedFlight3 = tick updatedFlight2
        let updatedFlight4 = tick updatedFlight3
        let updatedFlight5 = tick updatedFlight4
        let updatedFlight6 = tick updatedFlight5
        let updatedFlight7 = tick updatedFlight6

        let expectedFlight = Flight {plane = plane, Flight.from = fromDestination, Flight.to = toDestination, currentPosition = toDestination}
        assertEqual "Flight should be ended" expectedFlight updatedFlight7
    )

sqlTest :: Test
sqlTest =
  TestCase
    ( do
        connection <- connectODBC "DRIVER={ODBC Driver 17 for SQL Server};Server=localhost, 1499;Database=SimulatorService;Uid=sa;Pwd=Secret!Passw0rd;Connection Timeout=30"
        flights <- getAllFlights connection
        print ("result: " ++ show flights)
    )

tests :: Test
tests =
  TestList
    [ TestLabel "Should calculate remaining flight distance" shouldCalculateRemainingFlightDistance,
      TestLabel "Flight should reach its destination" flightShouldReachItsDestinationByTicking,
      TestLabel "sql test" sqlTest
    ]
