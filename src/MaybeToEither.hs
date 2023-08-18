module MaybeToEither (maybeToEither) where

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither e m =
  case m of
    Just value -> Right value
    Nothing -> Left e