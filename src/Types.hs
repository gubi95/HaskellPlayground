module Types (RawExpression, Parser, ParserOutput, Score, ParsedToken (..), createParsedToken) where

data ParsedToken
  = Plus
  | Minus
  | Number Int

instance Eq ParsedToken where
  (==) Plus Plus = True
  (==) Minus Minus = True
  (==) (Number n1) (Number n2) = n1 == n2
  (==) _ _ = False

createParsedToken :: Char -> Maybe ParsedToken
createParsedToken c =
  case c of
    '+' -> Just Plus
    '-' -> Just Minus
    '0' -> Just (Number 0)
    '1' -> Just (Number 1)
    '2' -> Just (Number 2)
    '3' -> Just (Number 3)
    '4' -> Just (Number 4)
    '5' -> Just (Number 5)
    '6' -> Just (Number 6)
    '7' -> Just (Number 7)
    '8' -> Just (Number 8)
    '9' -> Just (Number 9)
    _ -> Nothing

type ParserOutput = [ParsedToken]

type RawExpression = String

type Parser = RawExpression -> Maybe ParserOutput

type Score = Int