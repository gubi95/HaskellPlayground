module Main (main) where

import EquationParser
import EquationSolver

main :: IO ()
main = do
  putStrLn "Insert expression to solve"
  rawExpression <- getLine
  let result = solve rawExpression EquationParser.parse

  print $ either ("Error: " ++) (("Score: " ++) . show) result