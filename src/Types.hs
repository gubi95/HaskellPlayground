module Types (RawExpression, Parser, ParserError, ParserOutput, Score, ParsedToken (..), Operator (..), createParsedToken, createNumberParsedToken) where

data Operator
  = Plus
  | Minus
  | Number Int

instance Eq Operator where
  (==) Plus Plus = True
  (==) Minus Minus = True
  (==) (Number n1) (Number n2) = n1 == n2
  (==) _ _ = False

instance Show Operator where
  show Plus = "+"
  show Minus = "-"
  show (Number n) = show n

data ParsedToken
  = Operator Operator
  | LeftBracket
  | RightBracket

instance Eq ParsedToken where
  (==) (Operator o1) (Operator o2) = o1 == o2
  (==) LeftBracket LeftBracket = True
  (==) RightBracket RightBracket = True
  (==) _ _ = False

instance Show ParsedToken where
  show (Operator o) = show o
  show LeftBracket = "("
  show RightBracket = ")"

createParsedToken :: Char -> Either String ParsedToken
createParsedToken c =
  case c of
    '+' -> Right (Operator Plus)
    '-' -> Right (Operator Minus)
    '0' -> Right (Operator (Number 0))
    '1' -> Right (Operator (Number 1))
    '2' -> Right (Operator (Number 2))
    '3' -> Right (Operator (Number 3))
    '4' -> Right (Operator (Number 4))
    '5' -> Right (Operator (Number 5))
    '6' -> Right (Operator (Number 6))
    '7' -> Right (Operator (Number 7))
    '8' -> Right (Operator (Number 8))
    '9' -> Right (Operator (Number 9))
    ')' -> Right RightBracket
    '(' -> Right LeftBracket
    _ -> Left $ "Invalid character: " ++ [c]

createNumberParsedToken :: String -> Either String Operator
createNumberParsedToken rawNumber =
  Right $ Number (read rawNumber :: Int)

type ParserOutput = [Operator]

type RawExpression = String

type ParserError = String

type Parser = RawExpression -> Either ParserError ParserOutput

type Score = Int