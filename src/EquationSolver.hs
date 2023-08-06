module EquationSolver
  ( solve,
    Expression (..),
  )
where

import Data.Stack (Stack, stackNew, stackPop, stackPush)
import Types (ParsedToken (..), Parser, ParserOutput, RawExpression, Score)

data Expression
  = Add (Int, Expression)
  | Subtract (Int, Expression)
  | RawNumber Int

calculateExpressions :: Stack Expression -> Maybe Score
calculateExpressions expressions = do
  expr <- fmap snd (stackPop expressions)
  Just (calculateExpression expr)

calculateExpression :: Expression -> Int
calculateExpression (Add (number, expression)) = number + calculateExpression expression
calculateExpression (Subtract (number, expression)) = number - calculateExpression expression
calculateExpression (RawNumber number) = number

buildExpression :: ParserOutput -> Stack Expression -> Maybe (ParserOutput, Stack Expression)
buildExpression [] stack = Just ([], stack)
buildExpression (first : rest) stack = do
  case first of
    Number number -> buildExpression rest (stackPush stack (RawNumber number))
    Plus -> do
      (newStack1, expr) <- stackPop stack
      (newStack2, number2) <- stackPop newStack1
      let addition = Add (calculateExpression expr, number2)
      buildExpression rest (stackPush newStack2 addition)
    Minus -> do
      (newStack1, expr) <- stackPop stack
      (newStack2, number2) <- stackPop newStack1
      let subtraction = Subtract (calculateExpression number2, expr)
      buildExpression rest (stackPush newStack2 subtraction)

solve :: RawExpression -> Parser -> Maybe Score
solve rawExpression parser = do
  output <- parser rawExpression
  expressions <- fmap snd (buildExpression output stackNew)
  calculateExpressions expressions
