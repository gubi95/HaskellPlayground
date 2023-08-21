module EquationParser
  ( parse,
  )
where

import Data.Stack (Stack, stackNew, stackPop, stackPush)
import MaybeToEither (maybeToEither)
import Types

popOperatorsAndAddToOutputUntilOperatorIsFound :: (Stack Char, ParserOutput) -> Either String (Stack Char, ParserOutput)
popOperatorsAndAddToOutputUntilOperatorIsFound (stack, output) = do
  let defaultReturn = Right (stack, output)

  case stackPop stack of
    Just (newStack, rawOperator) ->
      either
        (const defaultReturn)
        (\parsedToken -> popOperatorsAndAddToOutputUntilOperatorIsFound (newStack, output ++ [parsedToken]))
        (createParsedToken rawOperator)
    _ -> defaultReturn

popOperatorForRightBracket :: Stack Char -> ParserOutput -> Either String (Stack Char, ParserOutput)
popOperatorForRightBracket stack output = do
  let defaultReturn = Right (stack, output)

  either
    (const defaultReturn)
    ( \(newStack, operator) ->
        case (newStack, operator) of
          (_, '(') -> defaultReturn
          _ -> do
            parsedOperator <- createParsedToken operator
            Right (newStack, output ++ [parsedOperator])
    )
    (maybeToEither "Ignored" $ stackPop stack)

processChar :: Char -> (Stack Char, ParserOutput) -> Either String (Stack Char, ParserOutput)
processChar c (stack, output) = do
  let processOperator () = do
        (newStack, newOutput) <- popOperatorsAndAddToOutputUntilOperatorIsFound (stack, output)
        Right (stackPush newStack c, newOutput)

  case (c, createParsedToken c) of
    (_, Right (Number number)) -> Right (stack, output ++ [Number number])
    (_, Right Plus) -> processOperator ()
    (_, Right Minus) -> processOperator ()
    ('(', _) -> Right (stackPush stack '(', output)
    (')', _) -> do
      (newStack, newOutput) <- popOperatorForRightBracket stack output
      stackWithoutRightBracket <- fst <$> (maybeToEither "Stack is already empty when trying to pop right bracktet" . stackPop $ newStack)
      Right (stackWithoutRightBracket, newOutput)
    (character, Left e) -> Left $ "Character: " ++ [character] ++ " cannot be processed. Details: " ++ e

extractExpression :: (Stack Char, ParserOutput) -> RawExpression -> Either String (Stack Char, ParserOutput)
extractExpression input [] = Right input
extractExpression input (first : rest) = do
  processCharOutput <- processChar first input
  extractExpressionOutput <- extractExpression processCharOutput rest
  popOperatorsAndAddToOutputUntilOperatorIsFound extractExpressionOutput

parse :: RawExpression -> Either String ParserOutput
parse rawExpression = do
  snd <$> extractExpression (stackNew, []) rawExpression
