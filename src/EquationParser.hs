module EquationParser
  ( parse,
  )
where

import Data.Stack (Stack, stackNew, stackPop, stackPush)
import Types

popOperatorsAndAddToOutputUntilOperatorIsFound :: (Stack Char, ParserOutput) -> Maybe (Stack Char, ParserOutput)
popOperatorsAndAddToOutputUntilOperatorIsFound (stack, output) = do
  let def = Just (stack, output)

  maybe
    def
    ( \(newStack, rawOperator) -> do
        maybe
          def
          (\parsedToken -> popOperatorsAndAddToOutputUntilOperatorIsFound (newStack, output ++ [parsedToken]))
          (createParsedToken rawOperator)
    )
    (stackPop stack)

popOperatorsAndAddToOutputUntilOperatorIsRightBracket :: Stack Char -> ParserOutput -> Maybe (Stack Char, ParserOutput)
popOperatorsAndAddToOutputUntilOperatorIsRightBracket stack output = do
  let def = Just (stack, output)

  maybe
    def
    ( \(newStack, operator) ->
        case (newStack, operator) of
          (_, '(') -> def
          _ -> do
            parsedOperator <- createParsedToken operator
            Just (newStack, output ++ [parsedOperator])
    )
    (stackPop stack)

processChar :: Char -> (Stack Char, ParserOutput) -> Maybe (Stack Char, ParserOutput)
processChar c (stack, output) = do
  case (c, createParsedToken c) of
    (_, Just (Number number)) -> Just (stack, output ++ [Number number])
    (_, operator) | operator `elem` [Just Plus, Just Minus] -> do
      (newStack, newOutput) <- popOperatorsAndAddToOutputUntilOperatorIsFound (stack, output)
      Just (stackPush newStack c, newOutput)
    (leftBracket, _) | leftBracket == '(' -> Just (stackPush stack leftBracket, output)
    (')', _) -> do
      (newStack, newOutput) <- popOperatorsAndAddToOutputUntilOperatorIsRightBracket stack output
      stackWithoutRightBracket <- fmap fst (stackPop newStack)
      Just (stackWithoutRightBracket, newOutput)
    _ -> Nothing

extractExpression :: (Stack Char, ParserOutput) -> RawExpression -> Maybe (Stack Char, ParserOutput)
extractExpression input [] = Just input
extractExpression input (first : rest) = do
  processCharOutput <- processChar first input
  extractExpressionOutput <- extractExpression processCharOutput rest
  popOperatorsAndAddToOutputUntilOperatorIsFound extractExpressionOutput

parse :: RawExpression -> Maybe ParserOutput
parse rawExpression = do
  fmap snd (extractExpression (stackNew, []) rawExpression)
