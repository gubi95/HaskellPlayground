module MaybeToEither (maybeToEither) where

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e m =
  case m of
    Just value -> Right value
    Nothing -> Left e