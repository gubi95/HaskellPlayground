import JsonParser (JsonToken (..), parse)
import Test.HUnit
  ( Counts,
    Test (TestCase, TestLabel, TestList),
    assertEqual,
    runTestTT,
  )

main :: IO Counts
main =
  runTestTT $
    TestList
      [ TestLabel
          "Should parse single string property object"
          ( TestCase
              ( do
                  let json = "{\"test\":\"value\"}"

                  let expected =
                        Just
                          [ LeftCurlyBracket,
                            DoubleQuote,
                            Property "test",
                            DoubleQuote,
                            Colon,
                            DoubleQuote,
                            StringValue "value",
                            DoubleQuote,
                            RightCurlyBracket
                          ]

                  assertEqual "" expected (JsonParser.parse json)
              )
          ),
        TestLabel
          "Should parse single null property object"
          ( TestCase
              ( do
                  let json = "{\"test\":null}"

                  let expected =
                        Just
                          [ LeftCurlyBracket,
                            DoubleQuote,
                            Property "test",
                            DoubleQuote,
                            Colon,
                            NullValue,
                            RightCurlyBracket
                          ]

                  assertEqual "" expected (JsonParser.parse json)
              )
          )
      ]
