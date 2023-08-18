module Main (main) where

import EquationParser
import EquationSolver

main :: IO ()
main = do
  putStrLn "Insert expression to solve"
  let rawExpression = "1+(2-3)+((4-5)+6+(7-8))+9+(1+4)+1" -- <- getLine
  let result = solve rawExpression EquationParser.parse

  case result of
    Right score -> do
      print $ "Score: " ++ show score
    Left e -> print $ "Error: " ++ e