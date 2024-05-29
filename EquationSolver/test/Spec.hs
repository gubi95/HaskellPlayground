import EquationParser
import Test.HUnit
import Types (Operator (..))

givenRawExpressionShouldReturnCorrentOutput :: Test
givenRawExpressionShouldReturnCorrentOutput =
  TestCase
    ( do
        let expression = "1+(150-200)+((20+(10)+80)-(160-50))+88"
        let actualResult = EquationParser.parse expression
        let expectedResult =
              Right
                [ Number 1,
                  Number 150,
                  Number 200,
                  Minus,
                  Plus,
                  Number 20,
                  Number 10,
                  Plus,
                  Number 80,
                  Plus,
                  Number 160,
                  Number 50,
                  Minus,
                  Minus,
                  Plus,
                  Number 88,
                  Plus
                ]
        assertEqual "output should be equal" expectedResult actualResult
    )

givenExpressionWithInvalidCharacterShouldReturnError :: Test
givenExpressionWithInvalidCharacterShouldReturnError =
  TestCase
    ( do
        let expression = "1+(150-200)+((20+(1A)+80)-(160-50))+88"
        let actualResult = EquationParser.parse expression
        let expectedResult = Left "Invalid character: A"
        assertEqual "output should be equal" expectedResult actualResult
    )

tests :: Test
tests =
  TestList
    [ TestLabel "Given raw expression Should return corrent output" givenRawExpressionShouldReturnCorrentOutput,
      TestLabel "Given expression with invalid character Should return error" givenExpressionWithInvalidCharacterShouldReturnError
    ]

main :: IO Counts
main = do
  runTestTT tests