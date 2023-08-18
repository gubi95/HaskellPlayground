module EquationSolver
  ( solve,
    Expression (..),
  )
where

import Data.Stack (Stack, stackNew, stackPop, stackPush)
import MaybeToEither (maybeToEither)
import Types (ParsedToken (..), Parser, ParserOutput, RawExpression, Score)

data Expression
  = Add (Int, Expression)
  | Subtract (Int, Expression)
  | RawNumber Int

calculateExpressions :: Stack Expression -> Either String Score
calculateExpressions expressions = do
  expr <- snd <$> maybeToEither "Stack is already empty" (stackPop expressions)
  Right (calculateExpression expr)

calculateExpression :: Expression -> Int
calculateExpression (Add (number, expression)) = number + calculateExpression expression
calculateExpression (Subtract (number, expression)) = number - calculateExpression expression
calculateExpression (RawNumber number) = number

buildExpression :: ParserOutput -> Stack Expression -> Either String (ParserOutput, Stack Expression)
buildExpression [] stack = Right ([], stack)
buildExpression (first : rest) stack = do
  case first of
    Number number -> buildExpression rest (stackPush stack (RawNumber number))
    Plus -> do
      (newStack1, expr) <- maybeToEither "Stack is already empty" . stackPop $ stack
      (newStack2, number2) <- maybeToEither "Stack is already empty" . stackPop $ newStack1
      let addition = Add (calculateExpression expr, number2)
      buildExpression rest (stackPush newStack2 addition)
    Minus -> do
      (newStack1, expr) <- maybeToEither "Stack is already empty" . stackPop $ stack
      (newStack2, number2) <- maybeToEither "Stack is already empty" . stackPop $ newStack1
      let subtraction = Subtract (calculateExpression number2, expr)
      buildExpression rest (stackPush newStack2 subtraction)

solve :: RawExpression -> Parser -> Either String Score
solve rawExpression parser = do
  output <- parser rawExpression
  expressions <- snd <$> buildExpression output stackNew
  calculateExpressions expressions
