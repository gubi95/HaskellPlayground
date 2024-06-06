module Queue (Queue (Queue), push, pop, singleton) where

newtype Queue t = Queue [t]

push :: Queue t -> t -> Queue t
push (Queue q) e = Queue (q ++ [e])

pop :: Queue t -> (Maybe t, Queue t)
pop (Queue []) = (Nothing, Queue [])
pop (Queue xs) = (Just $ head xs, Queue $ tail xs)

singleton :: t -> Queue t
singleton t = Queue [t]