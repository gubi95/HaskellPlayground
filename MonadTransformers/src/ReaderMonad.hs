{-# LANGUAGE InstanceSigs #-}

module ReaderMonad (Reader (..)) where

newtype Reader configuration a = Reader {runReader :: configuration -> a}

instance Monad (Reader configuration) where
  (>>=) :: Reader configuration a -> (a -> Reader configuration b) -> Reader configuration b
  (>>=) reader action = do
    Reader $ \configuration -> do
      let result = runReader reader configuration
      let nextResult = action result
      runReader nextResult configuration

instance Applicative (Reader configuration) where
  pure :: a -> Reader configuration a
  pure a = Reader $ const a
  (<*>) :: Reader configuration (a -> b) -> Reader configuration a -> Reader configuration b
  (<*>) action reader = Reader $ \configuration -> do
    let result = runReader reader configuration
    let resultF = runReader action configuration
    resultF result

instance Functor (Reader configuration) where
  fmap :: (a -> b) -> Reader configuration a -> Reader configuration b
  fmap f reader = Reader $ \configuration -> do
    let result = runReader reader configuration
    f result