{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module TreeDrawer (draw) where

import BinaryTree (TreeNode (..), TreeRoot, getNodesFromLevel, getTreeHeight)
import Control.Monad (forM_)
import GHC.Base (returnIO)

type TreeLevel = Int

drawRow :: TreeLevel -> TreeNode -> IO ()
drawRow lvl root = do
  let nodes = getNodesFromLevel root lvl

  forM_
    nodes
    (\node -> putStr (show (value node) ++ " "))

  putStrLn ""

drawRows :: TreeLevel -> TreeLevel -> TreeNode -> IO ()
drawRows lvl maxLvl node =
  case lvl > maxLvl of
    True -> returnIO ()
    _ -> do
      drawRow lvl node
      drawRows (lvl + 1) maxLvl node

draw :: TreeRoot -> IO ()
draw root = do
  let treeHeight = getTreeHeight root
  let maxLevel = treeHeight - 1
  drawRows 0 maxLevel root
