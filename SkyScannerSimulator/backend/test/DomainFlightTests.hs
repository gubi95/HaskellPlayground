module DomainFlightTests (tests) where

import Coordinates (Coordinates (..), createCoordinateValue)
import Flight (Airport (..), Flight (..), calculateIntermediatePoint, getRemainingDistance, tick)
import Plane
import Test.HUnit

shouldCalculateRemainingFlightDistanceTest :: Test
shouldCalculateRemainingFlightDistanceTest =
  TestCase $
    do
      let flight =
            Flight
              { Flight.id = 1,
                plane = Plane {kind = AirbusA380, kilometersPerTick = 10.0},
                from = Airport {location = Coordinates {lat = createCoordinateValue 51.5, lon = createCoordinateValue 0.0}, code = "C1"},
                to = Airport {location = Coordinates {lat = createCoordinateValue 38.8, lon = createCoordinateValue (-77.1)}, code = "C2"},
                currentPosition = Coordinates {lat = createCoordinateValue 51.5, lon = createCoordinateValue 0.0},
                progress = 0.0
              }

      let actualRemainingDistance = getRemainingDistance flight

      assertEqual "Should calculate remaining distance correctly" 5918 actualRemainingDistance

shouldCalculateIntermediatePointAsStartingPointTest :: Test
shouldCalculateIntermediatePointAsStartingPointTest =
  TestCase $
    do
      let fromDestination = Coordinates {lat = createCoordinateValue 40.646149, lon = createCoordinateValue (-73.785964)}
      let toDestination = Coordinates {lat = createCoordinateValue 33.940325, lon = createCoordinateValue (-118.412331)}

      let actualIntermediatePoint = calculateIntermediatePoint fromDestination toDestination 0.0

      assertEqual "Should calculate intermediate point as starting point" fromDestination actualIntermediatePoint

shouldCalculateIntermediatePointAsEndingPointTest :: Test
shouldCalculateIntermediatePointAsEndingPointTest =
  TestCase $
    do
      let fromDestination = Coordinates {lat = createCoordinateValue 40.646149, lon = createCoordinateValue (-73.785964)}
      let toDestination = Coordinates {lat = createCoordinateValue 33.940325, lon = createCoordinateValue (-118.412331)}

      let actualIntermediatePoint = calculateIntermediatePoint fromDestination toDestination 1.0

      assertEqual "Should calculate intermediate point correctly" toDestination actualIntermediatePoint

shouldCalculateIntermediatePointTest :: Test
shouldCalculateIntermediatePointTest =
  TestCase $
    do
      let fromDestination = Coordinates {lat = createCoordinateValue 40.646149, lon = createCoordinateValue (-73.785964)}
      let toDestination = Coordinates {lat = createCoordinateValue 33.940325, lon = createCoordinateValue (-118.412331)}

      let actualIntermediatePoint = calculateIntermediatePoint fromDestination toDestination 0.75
      let expectedIntermediatePoint = Coordinates {lat = createCoordinateValue 37.10550, lon = createCoordinateValue (-108.43188)}

      assertEqual "Should calculate intermediate point correctly" expectedIntermediatePoint actualIntermediatePoint

flightShouldReachItsDestinationByTickingTest :: Test
flightShouldReachItsDestinationByTickingTest =
  TestCase $
    do
      let p = Plane {kind = AirbusA380, kilometersPerTick = 10.0}
      let fromDestination = Airport {location = Coordinates {lat = createCoordinateValue 47.20761, lon = createCoordinateValue 27.02185}, code = "C1"}
      let toDestination = Airport {location = Coordinates {lat = createCoordinateValue 47.80543, lon = createCoordinateValue 27.02177}, code = "C2"}
      let flight =
            Flight
              { Flight.id = 1,
                plane = p,
                from = fromDestination,
                to = toDestination,
                currentPosition = location fromDestination,
                progress = 0.0
              }

      let actualFlight = iterate tick flight !! 7

      let expectedFlight =
            Flight
              { Flight.id = 1,
                plane = p,
                from = fromDestination,
                to = toDestination,
                currentPosition = location toDestination,
                progress = 1.0
              }

      assertEqual "Flight should be ended" expectedFlight actualFlight

tests :: Test
tests =
  TestList
    [ TestLabel "Should calculate remaining flight distance" shouldCalculateRemainingFlightDistanceTest,
      TestLabel "Shoul calculate intermediate point as starting point" shouldCalculateIntermediatePointAsStartingPointTest,
      TestLabel "Shoul calculate intermediate point as ending point" shouldCalculateIntermediatePointAsEndingPointTest,
      TestLabel "Should calculate intermediate point" shouldCalculateIntermediatePointTest,
      TestLabel "Flight should reach its destination" flightShouldReachItsDestinationByTickingTest
    ]
