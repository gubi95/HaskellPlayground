import Test.HUnit
import DomainFlightTests
import SqlFlightAdapterTests

main :: IO Counts
main = do
    runTestTT (
        TestList [
            TestLabel "Domain.Flight tests" DomainFlightTests.tests,
            TestLabel "Sql.FlightAdapter tests" SqlFlightAdapterTests.tests])
  
