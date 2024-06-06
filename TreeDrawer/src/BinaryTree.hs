{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module BinaryTree
  ( TreeNode (..),
    TreeRoot,
    add,
    getTreeHeight,
    getNodesFromLevel,
  )
where

import Queue

type TreeNodeValue = Int

type TreeLevel = Int

type TreeRoot = TreeNode

data TreeNode = TreeNode
  { value :: TreeNodeValue,
    left :: Maybe TreeNode,
    right :: Maybe TreeNode,
    level :: TreeLevel
  }

createEmptyNode :: TreeLevel -> TreeNodeValue -> TreeNode
createEmptyNode level value =
  TreeNode
    { value = value,
      left = Nothing,
      right = Nothing,
      level = level
    }

addWithLevel :: TreeLevel -> TreeNodeValue -> Maybe TreeRoot -> TreeNode
addWithLevel treeLevel newValue maybeRoot = do
  case maybeRoot of
    Nothing ->
      createEmptyNode
        treeLevel
        newValue
    Just root ->
      case newValue < value root of
        True -> root {left = Just $ addWithLevel (level root + 1) newValue (left root)}
        False -> root {right = Just $ addWithLevel (level root + 1) newValue (right root)}

add :: TreeNodeValue -> Maybe TreeRoot -> TreeNode
add = addWithLevel 0

calculateTreeHeight :: Maybe TreeNode -> Int
calculateTreeHeight maybeNode = do
  case maybeNode of
    Just node -> do
      let leftHeight = calculateTreeHeight (left node)
      let rightHeight = calculateTreeHeight (right node)
      max leftHeight rightHeight + 1
    _ -> 0

getTreeHeight :: TreeRoot -> Int
getTreeHeight root = calculateTreeHeight (Just root)

calculateNodesFromLevel :: TreeLevel -> Queue TreeNode -> [TreeNode] -> [TreeNode]
calculateNodesFromLevel desiredLevel q acc = do
  let (dequeued, queue) = pop q

  case dequeued of
    Nothing -> acc
    Just current -> do
      let currentLevel = level current

      let newQueue =
            case [left current, right current] of
              [Just l, Just r] -> foldl push queue [l, r]
              [Just l, Nothing] -> push queue l
              [Nothing, Just r] -> push queue r
              _ -> queue

      case desiredLevel == currentLevel of
        True -> calculateNodesFromLevel desiredLevel newQueue (acc ++ [current])
        _ -> calculateNodesFromLevel desiredLevel newQueue acc

getNodesFromLevel :: TreeRoot -> TreeLevel -> [TreeNode]
getNodesFromLevel root level = calculateNodesFromLevel level (singleton root) []