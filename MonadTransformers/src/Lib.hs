module Lib
  ( example,
    runReaderExample,
  )
where

import Control.Monad.Trans (lift)
import MaybeTransformer
import ReaderMonad

getMaybeLine :: () -> MaybeT IO String
getMaybeLine () = MaybeT $ do
  line <- getLine
  if line == ""
    then return Nothing
    else return $ Just line

process :: IO (Maybe String)
process = runMaybeT $ do
  lift $ putStrLn "Enter line 1:" :: MaybeT IO ()
  line1 <- getMaybeLine ()
  lift $ putStrLn "Enter line 2:"
  line2 <- getMaybeLine ()
  return $ line1 ++ " " ++ line2

example :: IO ()
example = do
  maybeLine <- process

  case maybeLine of
    Nothing -> putStrLn "No input provided."
    Just line -> putStrLn $ "You entered: " ++ line

  return ()

data MyConfiguration = MyConfiguration
  { value1 :: Int,
    value2 :: Int
  }

getValue1FromConfig :: MyConfiguration -> Int
getValue1FromConfig = value1

getValue2FromConfig :: MyConfiguration -> Int
getValue2FromConfig = value2

exampleReader :: Reader MyConfiguration Int
exampleReader = do
  val1 <- Reader getValue1FromConfig
  val2 <- Reader getValue2FromConfig

  return $ val1 + val2

runReaderExample :: IO ()
runReaderExample = do
  let config = MyConfiguration {value1 = 10, value2 = 20}
  let result = runReader exampleReader config
  putStrLn $ "The sum of values from configuration is: " ++ show result