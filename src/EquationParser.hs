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

popOperatorsAndAddToOutputUntilOperatorIsRightBracket :: Stack Char -> ParserOutput -> Either String (Stack Char, ParserOutput)
popOperatorsAndAddToOutputUntilOperatorIsRightBracket stack output = do
  let defaultReturn = Right (stack, output)

  maybe
    defaultReturn
    ( \(newStack, operator) ->
        case (newStack, operator) of
          (_, '(') -> defaultReturn
          _ -> do
            parsedOperator <- createParsedToken operator
            Right (newStack, output ++ [parsedOperator])
    )
    (stackPop stack)

processChar :: Char -> (Stack Char, ParserOutput) -> Either String (Stack Char, ParserOutput)
processChar c (stack, output) = do
  case (c, createParsedToken c) of
    (_, Right (Number number)) -> Right (stack, output ++ [Number number])
    (_, operator) | operator `elem` [Right Plus, Right Minus] -> do
      (newStack, newOutput) <- popOperatorsAndAddToOutputUntilOperatorIsFound (stack, output)
      Right (stackPush newStack c, newOutput)
    (leftBracket, _) | leftBracket == '(' -> Right (stackPush stack leftBracket, output)
    (')', _) -> do
      (newStack, newOutput) <- popOperatorsAndAddToOutputUntilOperatorIsRightBracket stack output
      stackWithoutRightBracket <- fst <$> (maybeToEither "Stack is already empty when trying to pop right bracktet" . stackPop $ newStack)
      Right (stackWithoutRightBracket, newOutput)
    (character, _) -> Left $ "Character: " ++ [character] ++ "cannot be processed"

extractExpression :: (Stack Char, ParserOutput) -> RawExpression -> Either String (Stack Char, ParserOutput)
extractExpression input [] = Right input
extractExpression input (first : rest) = do
  processCharOutput <- processChar first input
  extractExpressionOutput <- extractExpression processCharOutput rest
  popOperatorsAndAddToOutputUntilOperatorIsFound extractExpressionOutput

parse :: RawExpression -> Either String ParserOutput
parse rawExpression = do
  snd <$> extractExpression (stackNew, []) rawExpression
