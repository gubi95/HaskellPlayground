module Main (main) where

import BinaryTree (add)
import TreeDrawer (draw)

main :: IO ()
main = do
  let node1 = add 5 Nothing
  let node = add 10 (Just node1)
  let node3 = add 15 (Just node)
  let node4 = add 3 (Just node3)
  let node5 = add 25 (Just node4)
  let node6 = add 1 (Just node5)
  let node7 = add 2 (Just node6)
  let node8 = add 8 (Just node7)

  TreeDrawer.draw node8