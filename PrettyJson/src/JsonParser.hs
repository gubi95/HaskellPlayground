module JsonParser (parse) where

import Debug.Trace (traceM)

data Location
  = Start
  | End
  | Object
  | PropertyName
  | AfterPropertyName
  | AfterColon
  | StringPropertyValue
  | AfterPropertyValue
  deriving (Eq, Ord, Show)

data JsonToken
  = LeftCurlyBracket
  | RightCurlyBracket
  | DoubleQuote
  | Colon
  | Property String
  | StringValue String
  deriving (Eq, Ord, Show)

possiblePasses :: Location -> JsonToken -> Maybe Location
possiblePasses Start LeftCurlyBracket = Just Object
possiblePasses Object DoubleQuote = Just PropertyName
possiblePasses PropertyName (Property _) = Just PropertyName
possiblePasses PropertyName DoubleQuote = Just AfterPropertyName
possiblePasses AfterPropertyName Colon = Just AfterColon
possiblePasses AfterColon DoubleQuote = Just StringPropertyValue
possiblePasses StringPropertyValue (StringValue _) = Just StringPropertyValue
possiblePasses StringPropertyValue DoubleQuote = Just AfterPropertyValue
possiblePasses AfterPropertyValue RightCurlyBracket = Just End
possiblePasses _ _ = Nothing

parse :: String -> IO ()
parse rawJson = do
  let result = _parse (rawJson, Start)
  case result of
    Just _ -> putStrLn "Ok"
    Nothing -> putStrLn "Not ok"
  where
    _parse :: (String, Location) -> Maybe ([Char], Location)
    _parse (json, currentLocation) = do
      _ <- traceM ("Json: " ++ show json)
      _ <- traceM ("Current location: " ++ show currentLocation)

      case currentLocation of
        End -> Just ("", End)
        _ -> do
          (token, left) <- getNextToken json currentLocation

          _ <- traceM ("Next token: " ++ show token)
          _ <- traceM ("Left: " ++ show left)

          expectedNextLocation <- possiblePasses currentLocation token

          _parse (left, expectedNextLocation)

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
        _ -> Nothing

    getUntilDoubleQuote :: (String, String) -> Maybe (String, String)
    getUntilDoubleQuote (value, acc) =
      case acc of
        [] -> Nothing -- Left "No \" found"
        ['"'] -> Just (value, "\"")
        ('"' : t) -> Just (value, "\"" ++ t)
        (h : t) -> getUntilDoubleQuote (value ++ [h], t)