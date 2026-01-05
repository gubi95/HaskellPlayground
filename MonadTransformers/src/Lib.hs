module Lib
  ( example,
    runReaderExample,
    runStateExample,
  )
where

import Control.Monad.Trans (lift)
import MaybeTransformer
import ReaderMonad
import StateMonad


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


data CustomState = CustomState {
  health :: Int,
  mana :: Int
}
instance Show CustomState where
  show (CustomState h m) = "Health: " ++ show h ++ ", Mana: " ++ show m

addHealth :: State CustomState [String]
addHealth = State $ \s -> (["Added health"], s { health = health s + 1 })

addMana :: State CustomState [String]
addMana = State $ \s -> (["Added mana"], s { mana = mana s + 1 })

runStateExample :: IO ()
runStateExample = do
  let stateComputation = do
        _ <- addHealth
        _ <- addHealth
        _ <- addMana
        _ <- addHealth
        addMana

  let (lastOperation, state) = runState stateComputation (CustomState {health = 0, mana = 0})

  putStrLn $ "Operations performed: " ++ show lastOperation
  putStrLn $ "Final state and results: " ++ show state
