module Lib
    ( someFunc
    ) where

import JsonParser

someFunc :: IO ()
someFunc = parse "{\"test\":\"value\"}"
