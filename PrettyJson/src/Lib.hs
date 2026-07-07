module Lib
  ( someFunc,
  )
where

import JsonParser

someFunc :: IO ()
someFunc = do
  let tokens =
        parse
          "{\
          \\"test\":\
          \\"value\"\
          \}"
  putStrLn (show tokens)
