module DomainFlightTests (tests) where

import Coordinates (Coordinates (..))
import Flight (Flight (..), calculateIntermediatePoint, getRemainingDistance, tick)
import Plane
import Test.HUnit

shouldCalculateRemainingFlightDistanceTest :: Test
shouldCalculateRemainingFlightDistanceTest =
  TestCase $
    do
      let flight =
            Flight
              { plane = Plane {kind = AirbusA380, distancePerTick = 0},
                from = Coordinates {lat = 51.5, lon = 0.0},
                to = Coordinates {lat = 38.8, lon = -77.1},
                currentPosition = Coordinates {lat = 51.5, lon = 0.0},
                progress = 0.0
              }

      let actualRemainingDistance = getRemainingDistance flight

      assertEqual "Should calculate remaining distance correctly" 5918.185064088764 actualRemainingDistance

shouldCalculateIntermediatePointAsStartingPointTest :: Test
shouldCalculateIntermediatePointAsStartingPointTest =
  TestCase $
    do
      let fromDestination = Coordinates {lat = 40.646149, lon = -73.785964}
      let toDestination = Coordinates {lat = 33.940325, lon = -118.412331}

      let actualIntermediatePoint = calculateIntermediatePoint fromDestination toDestination 0.0

      assertEqual "Should calculate intermediate point as starting point" fromDestination actualIntermediatePoint

shouldCalculateIntermediatePointAsEndingPointTest :: Test
shouldCalculateIntermediatePointAsEndingPointTest =
  TestCase $
    do
      let fromDestination = Coordinates {lat = 40.646149, lon = -73.785964}
      let toDestination = Coordinates {lat = 33.940325, lon = -118.412331}

      let actualIntermediatePoint = calculateIntermediatePoint fromDestination toDestination 1.0

      assertEqual "Should calculate intermediate point correctly" toDestination actualIntermediatePoint

shouldCalculateIntermediatePointTest :: Test
shouldCalculateIntermediatePointTest =
  TestCase $
    do
      let fromDestination = Coordinates {lat = 40.646149, lon = -73.785964}
      let toDestination = Coordinates {lat = 33.940325, lon = -118.412331}

      let actualIntermediatePoint = calculateIntermediatePoint fromDestination toDestination 0.75
      let expectedIntermediatePoint = Coordinates {lat = 37.10551375881209, lon = -108.43186894098088}

      assertEqual "Should calculate intermediate point correctly" expectedIntermediatePoint actualIntermediatePoint

flightShouldReachItsDestinationByTickingTest :: Test
flightShouldReachItsDestinationByTickingTest =
  TestCase $
    do
      let p = Plane {kind = AirbusA380, distancePerTick = 0.15}
      let fromDestination = Coordinates {lat = 47.20761, lon = 27.02185}
      let toDestination = Coordinates {lat = 47.80543, lon = 27.02177}
      let flight =
            Flight
              { plane = p,
                from = fromDestination,
                to = toDestination,
                currentPosition = fromDestination,
                progress = 0.0
              }

      let updatedFlight1 = tick flight
      let updatedFlight2 = tick updatedFlight1
      let updatedFlight3 = tick updatedFlight2
      let updatedFlight4 = tick updatedFlight3
      let updatedFlight5 = tick updatedFlight4
      let updatedFlight6 = tick updatedFlight5
      let updatedFlight7 = tick updatedFlight6

      let expectedFlight =
            Flight
              { plane = p,
                from = fromDestination,
                to = toDestination,
                currentPosition = toDestination,
                progress = 1.0
              }

      assertEqual "Flight should be ended" expectedFlight updatedFlight7

tests :: Test
tests =
  TestList
    [ TestLabel "Should calculate remaining flight distance" shouldCalculateRemainingFlightDistanceTest,
      TestLabel "Shoul calculate intermediate point as starting point" shouldCalculateIntermediatePointAsStartingPointTest,
      TestLabel "Shoul calculate intermediate point as ending point" shouldCalculateIntermediatePointAsEndingPointTest,
      TestLabel "Should calculate intermediate point" shouldCalculateIntermediatePointTest,
      TestLabel "Flight should reach its destination" flightShouldReachItsDestinationByTickingTest
    ]
