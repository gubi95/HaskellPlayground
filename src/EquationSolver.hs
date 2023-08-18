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

popToEither :: Stack a -> Either String (Stack a, a)
popToEither = maybeToEither "Stack is already empty" . stackPop

calculateExpressions :: Stack Expression -> Either String Score
calculateExpressions expressions = do
  expr <- snd <$> popToEither expressions
  Right $ calculateExpression expr

calculateExpression :: Expression -> Int
calculateExpression (Add (number, expression)) = number + calculateExpression expression
calculateExpression (Subtract (number, expression)) = number - calculateExpression expression
calculateExpression (RawNumber number) = number

buildExpression :: ParserOutput -> Stack Expression -> Either String (ParserOutput, Stack Expression)
buildExpression [] stack = Right ([], stack)
buildExpression (token : tokens) stack = do
  let tokenToOperation expressionCtor = do
        (stack1, expr1) <- popToEither stack
        (stack2, expr2) <- popToEither stack1
        let operation = expressionCtor (calculateExpression expr1, expr2)
        buildExpression tokens $ stackPush stack2 operation

  case token of
    Number number -> buildExpression tokens (stackPush stack (RawNumber number))
    Plus -> tokenToOperation Add
    Minus -> tokenToOperation Subtract

solve :: RawExpression -> Parser -> Either String Score
solve rawExpression parser = do
  output <- parser rawExpression
  expressions <- snd <$> buildExpression output stackNew
  calculateExpressions expressions
