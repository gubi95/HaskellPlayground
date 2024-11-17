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
                currentPosition = Coordinates {lat = 51.5, lon = 0.0}
              }

      let actualRemainingDistance = getRemainingDistance flight

      assertEqual "Should calculate remaining distance correctly" 5918.185064088764 actualRemainingDistance

shouldCalculateIntermediatePointTest :: Test
shouldCalculateIntermediatePointTest =
  TestCase $
    do
      let from = Coordinates {lat = 47.20761, lon = 27.02185}
      let to = Coordinates {lat = 47.20754, lon = 27.02177}

      let actualIntermediatePoint = calculateIntermediatePoint from to 1.0
      let expectedIntermediatePoint = Coordinates {lat = 47.20754000000001, lon = 27.02177}

      assertEqual "Should calculate intermediate point correctly" expectedIntermediatePoint actualIntermediatePoint

flightShouldReachItsDestinationByTickingTest :: Test
flightShouldReachItsDestinationByTickingTest =
  TestCase $
    do
      let p = Plane {kind = AirbusA380, distancePerTick = 1.0}
      let fromDestination = Coordinates {lat = 47.20761, lon = 27.02185}
      let toDestination = Coordinates {lat = 47.20754, lon = 27.02177}
      let flight = Flight {plane = p, from = fromDestination, to = toDestination, currentPosition = fromDestination}

      -- updatedFlight1 <- tick flight
      -- updatedFlight2 <- tick updatedFlight1
      -- updatedFlight3 <- tick updatedFlight2
      -- let updatedFlight4 = tick updatedFlight3
      -- let updatedFlight5 = tick updatedFlight4
      -- let updatedFlight6 = tick updatedFlight5
      -- --let updatedFlight7 = tick updatedFlight6

      -- print $ "Remaining distance: " ++ show (getRemainingDistance flight)
      -- print $ "Remaining distance: " ++ show (getRemainingDistance updatedFlight1)
      -- print $ "Remaining distance: " ++ show (getRemainingDistance updatedFlight2)
      -- print $ "Remaining distance: " ++ show (getRemainingDistance updatedFlight3)

      -- let expectedFlight = Flight {plane = p, from = fromDestination, to = toDestination, currentPosition = toDestination}

      -- assertEqual "Flight should be ended" expectedFlight updatedFlight1

      pure ()

tests :: Test
tests =
  TestList
    [ TestLabel "Should calculate remaining flight distance" shouldCalculateRemainingFlightDistanceTest,
      TestLabel "Should calculate intermediate point" shouldCalculateIntermediatePointTest,
      TestLabel "Flight should reach its destination" flightShouldReachItsDestinationByTickingTest
    ]
