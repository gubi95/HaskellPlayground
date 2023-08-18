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

createParsedToken :: Char -> Either String ParsedToken
createParsedToken c =
  case c of
    '+' -> Right Plus
    '-' -> Right Minus
    '0' -> Right (Number 0)
    '1' -> Right (Number 1)
    '2' -> Right (Number 2)
    '3' -> Right (Number 3)
    '4' -> Right (Number 4)
    '5' -> Right (Number 5)
    '6' -> Right (Number 6)
    '7' -> Right (Number 7)
    '8' -> Right (Number 8)
    '9' -> Right (Number 9)
    _ -> Left $ "Invalid character: " ++ [c]

type ParserOutput = [ParsedToken]

type RawExpression = String

type Parser = RawExpression -> Either String ParserOutput

type Score = Int