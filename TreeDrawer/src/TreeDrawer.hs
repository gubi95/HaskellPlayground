module TreeDrawer (draw) where

import BinaryTree (TreeNode (..))
import GHC.Base (returnIO)

newtype Queue = Queue [TreeNode]

pushMany :: Queue -> [TreeNode] -> Queue
pushMany (Queue es) e = Queue (es ++ e)

push :: Queue -> TreeNode -> Queue
push q e = pushMany q [e]

pop :: Queue -> (Maybe TreeNode, Queue)
pop (Queue []) = (Nothing, Queue [])
pop (Queue xs) = (Just $ head xs, Queue $ tail xs)

type TreeLevel = Int

drawLevelOrder :: TreeLevel -> Queue -> IO ()
drawLevelOrder previousLevel q = do
  let (dequeued, queue) = pop q

  case dequeued of
    Nothing -> returnIO ()
    Just current -> do
      let currentLevel = level current
      
      if previousLevel == currentLevel
        then
          putStr (show (value current) ++ " ")
        else
          ( do
              putStrLn ""
              putStr (show (value current) ++ " ")
          )

      let newQueue =
            case [left current, right current] of
              [Just l, Just r] -> pushMany queue [l, r]
              [Just l, Nothing] -> push queue l
              [Nothing, Just r] -> push queue r
              _ -> queue

      drawLevelOrder currentLevel newQueue

draw :: TreeNode -> IO ()
draw node = drawLevelOrder (level node) (Queue [node])
