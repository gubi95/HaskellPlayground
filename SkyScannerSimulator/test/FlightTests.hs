module FlightTests (tests) where

import Coordinates
import Flight
import FlightAdapter
import Plane
import Test.HUnit

shouldCalculateRemainingFlightDistance :: Test
shouldCalculateRemainingFlightDistance =
  TestCase
    ( do
        let plane = Plane {kind = Airbus, distancePerTick = 0}
        let fromDestination = Coordinates {x = 0.0, y = 0.0}
        let toDestination = Coordinates {x = 4.0, y = 5.0}
        let current = Coordinates {x = 1.0, y = 1.0}
        let flight = Flight {plane = plane, from = fromDestination, to = toDestination, currentPosition = current}

        let actualRemainingDistance = getRemainingDistance flight
        let expectedDistance = 5.0

        assertEqual "Should calculare remaining distance correctly" expectedDistance actualRemainingDistance
    )

flightShouldReachItsDestinationByTicking :: Test
flightShouldReachItsDestinationByTicking =
  TestCase
    ( do
        let plane = Plane {kind = Airbus, distancePerTick = 1}
        let fromDestination = Coordinates {x = 0.0, y = 0.0}
        let toDestination = Coordinates {x = 4.0, y = 5.0}
        let flight = Flight {plane = plane, from = fromDestination, to = toDestination, currentPosition = fromDestination}

        let updatedFlight1 = tick flight
        let updatedFlight2 = tick updatedFlight1
        let updatedFlight3 = tick updatedFlight2
        let updatedFlight4 = tick updatedFlight3
        let updatedFlight5 = tick updatedFlight4
        let updatedFlight6 = tick updatedFlight5
        let updatedFlight7 = tick updatedFlight6

        let expectedFlight = Flight {plane = plane, from = fromDestination, to = toDestination, currentPosition = toDestination}
        assertEqual "Flight should be ended" expectedFlight updatedFlight7
    )

sqlTest :: Test
sqlTest =
  TestCase
    ( do
        a <- getAllFlights ()
        print ("result: " ++ show a)
    )

tests :: Test
tests =
  TestList
    [ TestLabel "Should calculate remaining flight distance" shouldCalculateRemainingFlightDistance,
      TestLabel "Flight should reach its destination" flightShouldReachItsDestinationByTicking,
      TestLabel "sql test" sqlTest
    ]
