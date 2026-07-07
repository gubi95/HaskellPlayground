import JsonParser (JsonToken (..), parse)
import Test.HUnit

main :: IO Counts
main =
  runTestTT $
    TestList
      [ TestLabel
          "Should parse sinle property object"
          ( TestCase
              ( do
                  let json =
                        "{\
                        \\"test\":\
                        \\"value\"\
                        \}"

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
          )
      ]
