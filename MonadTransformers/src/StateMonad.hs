{-# LANGUAGE InstanceSigs #-}

module StateMonad (State (..)) where

newtype State state a = State {runState :: state -> (a, state)}

instance Monad (State state) where
  (>>=) :: State state a -> (a -> State state b) -> State state b
  (>>=) state action = State $ \s -> do
    let (initResult, initState) = runState state s

    let (nextResult, nextState) = runState (action initResult) initState

    (nextResult, nextState)

instance Applicative (State state) where
  pure :: a -> State state a
  pure a = State $ \s -> (a, s)
  (<*>) :: State state (a -> b) -> State state a -> State state b
  (<*>) action state = State $ \s -> do
    let (f, initialState) = runState action s
    let (a, nextState) = runState state initialState
    (f a, nextState)

instance Functor (State state) where
  fmap :: (a -> b) -> State state a -> State state b
  fmap f state = State $ \s -> do
    let (a, nextState) = runState state s
    (f a, nextState)