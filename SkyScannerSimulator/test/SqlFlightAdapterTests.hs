module SqlFlightAdapterTests (tests) where

import Database.HDBC.ODBC (connectODBC)
import FlightAdapter (getAllFlights)
import Test.HUnit (Test (TestCase, TestLabel, TestList))

shouldReturnAllFlightsTest :: Test
shouldReturnAllFlightsTest =
  TestCase $
    do
      connection <- connectODBC "DRIVER={ODBC Driver 17 for SQL Server};Server=localhost, 1499;Database=SimulatorService;Uid=sa;Pwd=Secret!Passw0rd;Connection Timeout=30"
      flights <- getAllFlights connection
      print ("result: " ++ show flights)

tests :: Test
tests =
  TestList
    [ TestLabel "Should return all flights" shouldReturnAllFlightsTest
    ]