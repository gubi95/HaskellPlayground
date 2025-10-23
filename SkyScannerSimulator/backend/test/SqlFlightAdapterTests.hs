module SqlFlightAdapterTests (tests) where

import Coordinates
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import DeferredCleanup (DeferredCleanup, cleanupFlight)
import Flight
import FlightAdapter (createFlight, getAllFlights, updateFlight)
import Plane
import SqlAdapter (describeError)
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertBool, assertEqual)

shouldReturnAllFlightsTest :: String -> DeferredCleanup -> Test
shouldReturnAllFlightsTest connectionString deferredCleanup =
  TestCase $
    do
      connection <- connectPostgreSQL connectionString

      let flight1 =
            Flight
              { Flight.id = 0,
                plane = Plane {kind = AirbusA380, kilometersPerTick = 0},
                from = Airport {location = Coordinates {lat = createCoordinateValue 0, lon = createCoordinateValue 0}, code = "JFK"},
                to = Airport {location = Coordinates {lat = createCoordinateValue 0, lon = createCoordinateValue 0}, code = "LAX"},
                currentPosition = Coordinates {lat = createCoordinateValue 10, lon = createCoordinateValue 20},
                progress = 66
              }

      let flight2 =
            Flight
              { Flight.id = 0,
                plane = Plane {kind = Boeing777, kilometersPerTick = 0},
                from = Airport {location = Coordinates {lat = createCoordinateValue 0, lon = createCoordinateValue 0}, code = "SYD"},
                to = Airport {location = Coordinates {lat = createCoordinateValue 0, lon = createCoordinateValue 0}, code = "PEK"},
                currentPosition = Coordinates {lat = createCoordinateValue 30, lon = createCoordinateValue 40},
                progress = 88
              }

      createFlight1Result <- createFlight connection flight1
      let flight1Id = either (error . describeError) Prelude.id createFlight1Result
      cleanupFlight deferredCleanup flight1Id

      createFlight2Result <- createFlight connection flight2
      let flight2Id = either (error . describeError) Prelude.id createFlight2Result
      cleanupFlight deferredCleanup flight2Id

      getFlightsResult <- getAllFlights connection
      let actualFlights = either (error . describeError) Prelude.id getFlightsResult

      let actualFlight1 = filter (\flight -> Flight.id flight == flight1Id) actualFlights
      let actualFlight2 = filter (\flight -> Flight.id flight == flight2Id) actualFlights

      let expectedFlight1 =
            Flight
              { Flight.id = flight1Id,
                plane = Plane {kind = AirbusA380, kilometersPerTick = 12.0},
                from = Airport {location = Coordinates {lat = createCoordinateValue 40.64614, lon = createCoordinateValue (-73.78596)}, code = "JFK"},
                to = Airport {location = Coordinates {lat = createCoordinateValue 33.94032, lon = createCoordinateValue (-118.41233)}, code = "LAX"},
                currentPosition = Coordinates {lat = createCoordinateValue 10, lon = createCoordinateValue 20},
                progress = 66
              }

      let expectedFlight2 =
            Flight
              { Flight.id = flight2Id,
                plane = Plane {kind = Boeing777, kilometersPerTick = 10.0},
                from = Airport {location = Coordinates {lat = createCoordinateValue (-33.94611), lon = createCoordinateValue 151.17722}, code = "SYD"},
                to = Airport {location = Coordinates {lat = createCoordinateValue 40.08, lon = createCoordinateValue 116.58444}, code = "PEK"},
                currentPosition = Coordinates {lat = createCoordinateValue 30, lon = createCoordinateValue 40},
                progress = 88
              }

      assertBool "Should fetch at least 2 flights" (length actualFlights >= 2)
      assertEqual "Should return first flight" expectedFlight1 (head actualFlight1)
      assertEqual "Should return second flight" expectedFlight2 (head actualFlight2)

shouldUpdateFlightTest :: String -> DeferredCleanup -> Test
shouldUpdateFlightTest connectionString deferredCleanup =
  TestCase $
    do
      let flight =
            Flight
              { Flight.id = 0,
                plane = Plane {kind = AirbusA380, kilometersPerTick = 0},
                from = Airport {location = Coordinates {lat = createCoordinateValue 0, lon = createCoordinateValue 0}, code = "JFK"},
                to = Airport {location = Coordinates {lat = createCoordinateValue 0, lon = createCoordinateValue 0}, code = "LAX"},
                currentPosition = Coordinates {lat = createCoordinateValue 10, lon = createCoordinateValue 20},
                progress = 66
              }

      connection <- connectPostgreSQL connectionString
      createFlightResult <- createFlight connection flight
      let flightId = either (error . describeError) Prelude.id createFlightResult
      cleanupFlight deferredCleanup flightId

      let updatedFlight = flight {Flight.id = flightId, progress = 50, currentPosition = Coordinates {lat = createCoordinateValue 100, lon = createCoordinateValue 200}}
      _ <- either (error . describeError) Prelude.id <$> updateFlight connection updatedFlight

      let expectedFlight =
            Flight
              { Flight.id = flightId,
                plane = Plane {kind = AirbusA380, kilometersPerTick = 12.0},
                from = Airport {location = Coordinates {lat = createCoordinateValue 40.64614, lon = createCoordinateValue (-73.78596)}, code = "JFK"},
                to = Airport {location = Coordinates {lat = createCoordinateValue 33.94032, lon = createCoordinateValue (-118.41233)}, code = "LAX"},
                currentPosition = Coordinates {lat = createCoordinateValue 100, lon = createCoordinateValue 200},
                progress = 50
              }

      getFlightsResult <- getAllFlights connection
      let actualFlights = either (error . describeError) Prelude.id getFlightsResult

      let actualFlight = filter (\f -> Flight.id f == flightId) actualFlights

      assertEqual "Should return updated flight" [expectedFlight] actualFlight

tests :: String -> DeferredCleanup -> Test
tests connectionString deferredCleanup =
  TestList
    [ TestLabel "Should return all flights" $ shouldReturnAllFlightsTest connectionString deferredCleanup,
      TestLabel "Should update flight" $ shouldUpdateFlightTest connectionString deferredCleanup
    ]