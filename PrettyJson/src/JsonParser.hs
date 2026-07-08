module JsonParser (parse, JsonToken (..)) where

import Data.List (isPrefixOf)
import Debug.Trace (traceM)

data Location
  = Start
  | End
  | Object
  | PropertyName
  | AfterPropertyName
  | AfterColon
  | StringPropertyValue
  | NullPropertyValue
  | AfterPropertyValue
  deriving (Eq, Ord, Show)

data JsonToken
  = LeftCurlyBracket
  | RightCurlyBracket
  | DoubleQuote
  | Colon
  | Property String
  | StringValue String
  | NullValue
  | BooleanValue Bool
  deriving (Eq, Ord, Show)

possiblePasses :: Location -> JsonToken -> Maybe Location
possiblePasses Start LeftCurlyBracket = Just Object
possiblePasses Object DoubleQuote = Just PropertyName
possiblePasses PropertyName (Property _) = Just PropertyName
possiblePasses PropertyName DoubleQuote = Just AfterPropertyName
possiblePasses AfterPropertyName Colon = Just AfterColon
possiblePasses AfterColon DoubleQuote = Just StringPropertyValue
possiblePasses AfterColon NullValue = Just AfterPropertyValue
possiblePasses AfterColon (BooleanValue _) = Just AfterPropertyValue
possiblePasses StringPropertyValue (StringValue _) = Just StringPropertyValue
possiblePasses StringPropertyValue DoubleQuote = Just AfterPropertyValue
possiblePasses AfterPropertyValue RightCurlyBracket = Just End
possiblePasses _ _ = Nothing

parse :: String -> Maybe [JsonToken]
parse rawJson = do
  (_, _, tokens) <- _parse rawJson Start []
  return tokens
  where
    _parse :: String -> Location -> [JsonToken] -> Maybe ([Char], Location, [JsonToken])
    _parse json currentLocation tokens = do
      _ <- traceM ("Json: " ++ show json)
      _ <- traceM ("Current location: " ++ show currentLocation)

      case currentLocation of
        End -> Just ("", End, tokens)
        _ -> do
          (token, left) <- getNextToken json currentLocation

          _ <- traceM ("Next token: " ++ show token)
          _ <- traceM ("Left: " ++ show left)

          expectedNextLocation <- possiblePasses currentLocation token

          _parse left expectedNextLocation (tokens ++ [token])

    getNextToken :: String -> Location -> Maybe (JsonToken, String)
    getNextToken json currentLocation = do
      case (json, currentLocation) of
        ([], _) -> Nothing
        ('{' : t, Start) -> Just (LeftCurlyBracket, t)
        ('"' : t, Object) -> Just (DoubleQuote, t)
        ('"' : t, PropertyName) -> Just (DoubleQuote, t)
        (':' : t, AfterPropertyName) -> Just (Colon, t)
        ('"' : t, AfterColon) -> Just (DoubleQuote, t)
        ('"' : t, StringPropertyValue) -> Just (DoubleQuote, t)
        ('}' : t, AfterPropertyValue) -> Just (RightCurlyBracket, t)
        (h : t, PropertyName) -> fmap (\x -> (Property $ fst x, snd x)) $ getUntilDoubleQuote ("", h : t)
        (h : t, StringPropertyValue) -> fmap (\x -> (StringValue $ fst x, snd x)) $ getUntilDoubleQuote ("", h : t)
        (h : t, AfterColon) -> getNotDoubleQuotedValue (h : t)
        _ -> Nothing

    getUntilDoubleQuote :: (String, String) -> Maybe (String, String)
    getUntilDoubleQuote (value, acc) =
      case acc of
        [] -> Nothing -- Left "No \" found"
        ['"'] -> Just (value, "\"")
        ('"' : t) -> Just (value, "\"" ++ t)
        (h : t) -> getUntilDoubleQuote (value ++ [h], t)

    getNotDoubleQuotedValue :: String -> Maybe (JsonToken, String)
    getNotDoubleQuotedValue acc =
      case (getNull acc) of
        Just x -> Just x
        Nothing -> getBoolean acc

    getNull :: String -> Maybe (JsonToken, String)
    getNull acc =
      case (isPrefixOf "null" acc) of
        True -> Just (NullValue, drop 4 acc)
        False -> Nothing

    getBoolean :: String -> Maybe (JsonToken, String)
    getBoolean acc =
      case (isPrefixOf "true" acc) of
        True -> Just (BooleanValue True, drop 4 acc)
        False -> do
          case (isPrefixOf "false" acc) of
            True -> Just (BooleanValue False, drop 5 acc)
            False -> Nothing
