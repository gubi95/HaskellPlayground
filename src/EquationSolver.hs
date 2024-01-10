module EquationSolver
  ( solve,
    Expression (..),
  )
where

import Data.Stack (Stack, stackNew, stackPop, stackPush)
import MaybeToEither (maybeToEither)
import Types (Operator (..), Parser, ParserError, ParserOutput, RawExpression, Score)

data Expression
  = Add (Int, Expression)
  | Subtract (Int, Expression)
  | RawNumber Int

instance Show Expression where
  show (Add (n, expr)) = show n ++ "+" ++ show expr
  show (Subtract (n, expr)) = show n ++ "-" ++ show expr
  show (RawNumber n) = show n

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
buildExpression (operator : restOperators) stack = do
  let tokenToOperation expressionCtor = do
        (stack1, expr1) <- popToEither stack
        (stack2, expr2) <- popToEither stack1
        let operation = expressionCtor (calculateExpression expr2, expr1)
        buildExpression restOperators $ stackPush stack2 operation

  case operator of
    Number number -> buildExpression restOperators (stackPush stack (RawNumber number))
    Plus -> tokenToOperation Add
    Minus -> tokenToOperation Subtract

solve :: RawExpression -> Parser -> Either ParserError Score
solve rawExpression parser = do
  output <- parser rawExpression
  expressions <- snd <$> buildExpression output stackNew
  calculateExpressions expressions
