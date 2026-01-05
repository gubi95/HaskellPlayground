module Lib
    ( example
    ) where

import MaybeTransformer
import Control.Monad.Trans (lift)

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