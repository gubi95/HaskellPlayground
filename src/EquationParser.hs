module EquationParser
  ( parse,
  )
where

import Control.Applicative as List
import Data.Stack (Stack, stackNew, stackPop, stackPush)
import MaybeToEither (maybeToEither)
import Types
  ( Operator (..),
    ParsedToken (..),
    ParserOutput,
    RawExpression,
    createNumberParsedToken,
    createParsedToken,
  )

type NumberAccumulator = String

popAndAddToOutputUntilOperatorIsFound :: (Stack ParsedToken, ParserOutput, NumberAccumulator) -> (Stack ParsedToken, ParserOutput, NumberAccumulator)
popAndAddToOutputUntilOperatorIsFound (stack, output, acc) =
  case stackPop stack of
    Just (newStack, Operator operator) ->
      popAndAddToOutputUntilOperatorIsFound (newStack, output ++ [operator], acc)
    _ -> (stack, output, acc)

popOperatorForRightBracket :: (Stack ParsedToken, ParserOutput, NumberAccumulator) -> (Stack ParsedToken, ParserOutput, NumberAccumulator)
popOperatorForRightBracket (stack, output, acc) = do
  case stackPop stack of
    (Just (newStack, Operator operator)) -> (newStack, output ++ [operator], acc)
    _ -> (stack, output, acc)

processsToken :: ParsedToken -> (Stack ParsedToken, ParserOutput, NumberAccumulator) -> Either String (Stack ParsedToken, ParserOutput, NumberAccumulator)
processsToken token (stack, output, numberAcc) = do
  let processNumberFromAcc currentStack currentOutput =
        case numberAcc of
          [] ->
            Right (currentStack, currentOutput, [])
          _ -> do
            parsedNumber <- createNumberParsedToken numberAcc
            Right (currentStack, currentOutput ++ [parsedNumber], [])

  let processOperator () = do
        (newStack, newOutput, newNumberAcc) <- popAndAddToOutputUntilOperatorIsFound <$> processNumberFromAcc stack output
        Right (stackPush newStack token, newOutput, newNumberAcc)

  case token of
    Operator operator ->
      case operator of
        Number digit -> Right (stack, output, numberAcc ++ show digit)
        Plus -> processOperator ()
        Minus -> processOperator ()
    LeftBracket -> do
      (newStack, newOutput, newNumberAcc) <- processNumberFromAcc stack output
      Right (stackPush newStack LeftBracket, newOutput, newNumberAcc)
    RightBracket -> do
      (newStack, newOutput, newNumberAcc) <- popOperatorForRightBracket <$> processNumberFromAcc stack output
      stackWithoutRightBracket <- fst <$> (maybeToEither "Stack is already empty when trying to pop right bracket" . stackPop $ newStack)
      Right (stackWithoutRightBracket, newOutput, newNumberAcc)

extractExpression :: (Stack ParsedToken, ParserOutput, NumberAccumulator) -> [ParsedToken] -> Either String (Stack ParsedToken, ParserOutput, NumberAccumulator)
extractExpression (stack, output, numberAcc : numberAccN) [] = do
  parsedNumber <- createNumberParsedToken (numberAcc : numberAccN)
  Right (stack, output ++ [parsedNumber], [])
extractExpression input [] = Right input
extractExpression input (first : rest) = do
  (chars, output, newNumberAcc) <- processsToken first input
  extractExpressionOutput <- extractExpression (chars, output, newNumberAcc) rest
  Right $ popAndAddToOutputUntilOperatorIsFound extractExpressionOutput

parse :: RawExpression -> Either String ParserOutput
parse rawExpression = do
  tokens <- mapM createParsedToken rawExpression
  (_, output, _) <- extractExpression (stackNew, List.empty, List.empty) tokens
  Right output
