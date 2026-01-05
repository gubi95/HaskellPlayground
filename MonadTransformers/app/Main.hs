module Main (main) where

import Lib

main :: IO ()
main = do
  putStrLn "1 - MonadT run"
  putStrLn "2 - Reader Monad run"

  line <- getLine

  case line of
    "1" -> example
    "2" -> runReaderExample
    _ -> putStrLn "Invalid option. Please enter 1 or 2."
